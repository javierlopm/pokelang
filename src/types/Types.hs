module Types(
    Type(..),
    Declare(..),
    Message,
    TypeTuple,
    isEmpty,
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
    addType
    -- makeIter
    -- makeDec,
    -- isPointer,
    -- makeArr,
    -- makePtrs,
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
  show (Enum   (l,c) n   scp ) = "Enum("++show l++","++show c++ ") " ++ show n ++ " with scope: " ++ showScope 1 scp ++ "\n"
  show (Union  (l,c) n t scp ) = "Union("++show l++","++show c++ ") " ++ show n ++ " Type: " ++ show (TypeFunction t) ++" with scope: " ++ showScope 1 scp ++ "\n"
  show (Struct (l,c) n t scp ) = "Struct("++show l++","++show c++ ") " ++ show n ++ " Type: " ++ show (TypeFunction t) ++" with scope: " ++ showScope 1 scp ++ "\n"

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
          | TypePointer    Type
          | TypeEmptyArray Type
          | TypeArray      Type Int
          | TypeFunction   (Seq Type) 
          | TypeUndefined  -- Temporal
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
  show (TypePointer     t     ) = "Pointer to " ++ show t
  show (TypeEmptyArray  t     ) = "Array to "   ++ show t
  show (TypeArray       t dim ) = "Array size " ++ show dim ++ " of " ++ show t
  show (TypeFunction    l     ) = "(" ++ (concat . intersperse " × " . (map show) . toList) l ++ ")"


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
isFunc (Just (Function _ _ _)) = True
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

-- Check if declare is a pointer
-- isPointer :: Declare -> Bool
-- isPointer (Pointer _ _ _ _) = True
-- isPointer _                = False

-- Check if the declare type is Empty (forward declarations)
isEmpty :: Declare -> Bool
isEmpty Empty = True
isEmpty _     = False

-- Create declare with a Token an a position
-- makeDec :: Token -> Pos -> Maybe String -> Maybe Declare
-- makeDec (TkVoid _) _ _  = Nothing
-- makeDec t p (Just s)  = Just $
--     case t of 
--         TkStruct _ -> Variable p (TypeStruct s) (PrimStruct s)
--         TkUnion  _ -> Variable p (TypeUnion  s) (PrimUnion s)
--         TkEnum   _ -> Variable p (TypeEnum   s) (PrimEnum s)
-- makeDec t p Nothing = Just $
--     case t of 
--         TkInt    _ -> Variable p (TypeInt)   (PrimInt 0) 
--         TkBool   _ -> Variable p (TypeBool)  (PrimBool False)
--         TkChar   _ -> Variable p (TypeChar)  (PrimChar '\0')
--         TkFloat  _ -> Variable p (TypeFloat) (PrimFloat 0.0)

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

-- makeIter :: Token -> Pos->  Declare
-- makeIter (TkId _ _) (l,c)   = Variable  (l,c) TkInt True 
-- makeIter a  p               = error "what are you doing?"

-- Create a pointer declaration
-- makePtrs :: Token -> Pos -> Int -> Maybe String -> Maybe Declare
-- makePtrs t p i s = Just $
--     case t of 
--         TkInt    _ -> Pointer p TypeInt    i Nothing
--         TkBool   _ -> Pointer p TypeBool   i Nothing
--         TkChar   _ -> Pointer p TypeChar   i Nothing
--         TkFloat  _ -> Pointer p TypeFloat  i Nothing
--         TkVoid   _ -> Pointer p TypeVoid   i Nothing
--         TkStruct _ -> Pointer p TypeStruct i $ s
--         TkUnion  _ -> Pointer p TypeUnion  i $ s
--         TkEnum   _ -> Pointer p TypeEnum   i $ s

makeType :: Token -> Type
makeType (TkInt   _) = TypeInt
makeType (TkBool  _) = TypeBool
makeType (TkChar  _) = TypeChar
makeType (TkVoid  _) = TypeVoid
makeType (TkFloat _) = TypeFloat

makeDataType :: Token -> Token -> Type
makeDataType (TkStruct _ ) dataId =  TypeEnum   (lexeme dataId)
makeDataType (TkUnion  _ ) dataId =  TypeStruct (lexeme dataId)
makeDataType (TkEnum   _ ) dataId =  TypeUnion  (lexeme dataId)


type TypeTuple = Seq Type

emptytuple :: Seq Type 
emptytuple = empty

addType :: Seq Type -> Type -> Seq Type
addType = (|>)