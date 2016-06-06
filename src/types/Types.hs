module Types(
    Type(..),
    Declare(..),
    Message,
    TypeTuple,
    nums,
    isEmpty,
    isError,
    isReadable,
    isLIter,
    isFunc,
    toPointer,
    toEmptyArray,
    toArray,
    makeType,
    enumMatches,
    dataNameMatches,
    makeDataType,
    emptytuple,
    emptyTypeMatches,
    addType,
    sameData
) where

import Data.List(intersperse)
import TableTree(Scope(..),showScope)
import Data.Sequence(Seq,(|>),empty)
import Data.Foldable (toList)
import Tokens(Token(TkInt  ,TkBool ,TkChar
                   ,TkVoid ,TkFloat,TkStruct
                   ,TkUnion,TkEnum ,TkNull
                   ,TkDId,TkId,lexeme))

type Message = Either String String -- Monad writer message unit
type Pos = (Int,Int)

-- Las constantes enumeradas no deberian estar en un scope grande y universal?

-- Declarations might be functions,variables or structure types
data Declare = Function  { pos::Pos, storedType::Type, fields   ::(Scope Declare)}
             | Variable  { pos::Pos, storedType::Type, readonly :: Bool  } -- , storedTypeV::PrimType -- No se necesaita todavia
             | Cons      { pos::Pos } 
             | Struct    { pos::Pos, storedType :: Type, fieldTypes :: (Seq Type) , fields::(Scope Declare)}
             | Union     { pos::Pos, storedType :: Type, fieldTypes :: (Seq Type) , fields::(Scope Declare)} 
             | Enum      { pos::Pos, typeName ::String, fields::(Scope Declare)}
             | EnumCons  { pos::Pos, name :: String,ord  :: Int} 
             | EmptyWithType { storedType :: Type } -- Forward declare with type
             | Empty                                -- Empty forward declare

instance Show Declare where
  show (Function (l,c) t scope ) = 
      "Function ("++show l++","++show c++ ") Type:" ++show t ++ "" ++
        showScope 1 scope ++ "\n"
  show (Variable (l,c) t readonly ) = 
      "Variable ("++show l++","++show c++ ") Type:" ++show t++ " " ++ cons readonly
        where cons True = "| Iteration Var"
              cons _    = ""
  show (Cons (l,c)) = "Constant value"
  show (EnumCons (l,c) n ord) = "Enum Constant("++show l++","++show c++ ") \'" ++ n ++ "\' with cardinaly " ++ show ord
  show (EmptyWithType t) = "Forward Declaration of type "++ show t ++", this shouldn't be here" 
  show Empty  = " EMPTY " 
  show (Enum   (l,c) n   scp ) = "Enum("++show l++","++show c++ ") "   
                                  ++ "\nType for variables: Enum "++ n 
                                  ++ "\nScope: " ++ showScope 1 scp ++ "\n"
  show (Union  (l,c) n t scp ) = "Union("++show l++","++show c++ ") "  
                                  ++ "\nType for variables: " ++ show n
                                  ++ "\nDeclare Type: " ++ show (TypeFunction t) 
                                  ++ "\nScope: " ++ showScope 1 scp ++ "\n"
  show (Struct (l,c) n t scp ) = "Struct("++show l++","++show c++ ") " 
                                  ++ "\nType for variables: " ++ show n
                                  ++ "\nDeclare Type: " ++ show (TypeFunction t) 
                                  ++ "\nScope: " ++ showScope 1 scp ++ "\n"

-- Polymorphic store type
data PrimType = PrimInt        Int
              | PrimIter       Int
              | PrimBool       Bool
              | PrimChar       Char
              | PrimFloat      Float
              | PrimString     String
              | PrimEnum       String
              | PrimUnion      String
              | PrimStruct     String
              deriving(Show,Eq)

-- Language types for checks
data Type = TypeInt  
          | TypeBool   
          | TypeChar   
          | TypeFloat  
          | TypeVoid   
          | TypeEnum       String -- Name comparison 
          | TypeStruct     String 
          | TypeUnion      String
          | TypeField      String Type
          | TypePointer    Type
          | TypeEmptyArray Type
          | TypeArray      Type Int
          | TypeFunction   (Seq Type) 
          | TypeUndefined  -- Temporal
          | TypeError  
          deriving(Eq)

instance Show Type where
  show (TypeInt      ) = "Integer"
  show (TypeUndefined) = "NILL"
  show (TypeBool     ) = "Boolean"
  show (TypeChar     ) = "Character"
  show (TypeFloat    ) = "Float"
  show (TypeVoid     ) = "Void"
  show (TypeEnum   s ) = "Enum "   ++ s
  show (TypeUnion  s ) = "Union "  ++ s
  show (TypeStruct s ) = "Struct " ++ s
  show (TypeField  s t) = "(" ++ show t ++ " as " ++ s ++ ")"
  show (TypePointer     t     ) = "Pointer to " ++ show t
  show (TypeEmptyArray  t     ) = "Array to "   ++ show t
  show (TypeArray       t dim ) = "Array size " ++ show dim ++ " of " ++ show t
  show (TypeFunction    l     ) = "(" ++ (concat . intersperse "->" . (map show) . toList) l ++ ")"


-- lists
nums = [TypeInt,TypeFloat]

enumMatches :: Declare -> String -> Bool
enumMatches (Enum _ name _ ) str = name == str
enumMatches _ _                  = False

dataNameMatches :: Declare -> String -> Bool
dataNameMatches (Struct _ (TypeStruct name) _ _ ) str = name == str
dataNameMatches (Union  _ (TypeUnion name)  _ _ ) str = name == str
dataNameMatches  Empty   _             = True
dataNameMatches _ _                    = False

isFunc :: Maybe Declare -> Bool
isFunc Nothing = False
isFunc (Just (Function _ _ _))  = True
isFunc (Just (EmptyWithType _)) = True -- Forward declaration
isFunc (Just Empty) = True
isFunc a = False

isLIter :: Declare -> Bool
isLIter (Variable _ _ iterVar) = iterVar
isLIter _                      = False

--Checks if a declaration is readable
isReadable :: Maybe Declare -> Bool
isReadable (Just (Variable _ stType _)) = 
  case (stType) of
    TypeInt       -> True
    TypeBool      -> True
    TypeChar      -> True
    TypeFloat     -> True
    otherwise     -> False
isReadable a = False

-- Check if the declare type is Empty (forward declarations)
isEmpty :: Declare -> Bool
isEmpty Empty             = True
isEmpty (EmptyWithType _) = True
isEmpty _     = False

-- Check if forward declaration matches
emptyTypeMatches :: Declare -> TypeTuple -> Bool
emptyTypeMatches (EmptyWithType (TypeFunction t1)) t2 = t1 == t2
emptyTypeMatches Empty _ = True
emptyTypeMatches _ _     = False

{-
    Declare type transformation functions
-}

toPointer :: Declare -> Declare
toPointer dec = dec { storedType = TypePointer oldtype }
    where oldtype = storedType dec

toEmptyArray :: Declare -> Declare
toEmptyArray dec = dec { storedType = TypeEmptyArray oldtype }
    where oldtype = storedType dec

toArray  :: Declare -> Int -> Declare
toArray dec dim = dec { storedType = TypeArray oldtype dim }
    where oldtype = storedType dec

isError :: Type -> Bool
isError TypeError = True 
isError _         = False 

makeType :: Token -> Type
makeType (TkInt   _) = TypeInt
makeType (TkBool  _) = TypeBool
makeType (TkChar  _) = TypeChar
makeType (TkVoid  _) = TypeVoid
makeType (TkFloat _) = TypeFloat

makeDataType :: Token -> Token -> Type
makeDataType (TkStruct _ ) dataId =  TypeStruct (lexeme dataId)
makeDataType (TkUnion  _ ) dataId =  TypeUnion  (lexeme dataId)
makeDataType (TkEnum   _ ) dataId =  TypeEnum   (lexeme dataId)

sameData :: Token -> Type -> Bool
sameData (TkStruct _ )  (TypeStruct _ ) = True
sameData (TkUnion  _ )  (TypeUnion  _ ) = True
sameData (TkEnum   _ )  (TypeEnum   _ ) = True
sameData _              _               = False

type TypeTuple = Seq Type

emptytuple :: TypeTuple 
emptytuple = Data.Sequence.empty

addType :: TypeTuple -> Type -> TypeTuple
addType = (|>)