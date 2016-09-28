module Types(
    Direction(..),
    Type(..),
    Declare(..),
    Message,
    TypeTuple,
    nums,
    structured,
    isEmpty,
    isError,
    isReadable,
    isLIter,
    isLValue,
    isFunc,
    toPointer,
    isPointer,
    isBasic,
    toEmptyArray,
    toArray,
    makeType,
    enumMatches,
    dataNameMatches,
    makeDataType,
    emptyTypeMatches,
    sameData,
    isNotRecursiveData,
    -- Type Tuple Functions
    transformType,
    emptytuple,
    addType,
    funcReturnType,
    tuplesMatch,
    lengthMatches,
    makeTypeTuple,
    getSize,
    align,
    isAFunction,
    isEnumCons,
    notErrors
    -- addLeftType,
    -- singleType,
) where

import Data.List          (intersperse)
import TableTree          (Scope(..),showScope)
import qualified Data.Foldable as F (toList,all,foldl)
import Data.Sequence as S (Seq,(|>),ViewR(..),empty,viewr,zipWith,length,fromList)
import Tokens(Token(TkInt  ,TkBool ,TkChar
                   ,TkVoid ,TkFloat,TkStruct
                   ,TkUnion,TkEnum ,TkNull
                   ,TkDId,TkId,lexeme))

type Message = Either String String -- Monad writer message unit
type Pos = (Int,Int)

-- Las constantes enumeradas no deberian estar en un scope grande y universal?

data Direction = Label
               | Offset { bytes :: Int}
               deriving(Show)

-- Declarations might be functions,variables or structure types
data Declare = Function  { pos::Pos, storedType::Type, fields   ::(Scope Declare)}
             | Variable  { pos::Pos, storedType::Type, readonly :: Bool, dir :: Direction  } -- , storedTypeV::PrimType -- No se necesaita todavia
             | Cons      { pos::Pos } 
             | Struct    { pos::Pos, storedType :: Type, fieldTypes :: (Seq Type) , fields::(Scope Declare)}
             | Union     { pos::Pos, storedType :: Type, fieldTypes :: (Seq Type) , fields::(Scope Declare)} 
             | Enum      { pos::Pos, typeName   :: String, fields::(Scope Declare)}
             | EnumCons  { pos::Pos, storedDType:: String, name :: String,ord  :: Int} 
             | EmptyWithType { storedType :: Type } -- Forward declare with type
             | Empty                                -- Empty forward declare

instance Show Declare where
  show (Function (l,c) t scope ) = 
      "Function ("++show l++","++show c++ ") Type:" ++show t ++ "" ++
        showScope 1 scope ++ "\n"
  show (Variable (l,c) t readonly dir ) = 
      "Variable ("++show l++","++show c++ ") Type:" ++show t++ " at " ++ show dir ++ " " ++ cons readonly
        where cons True = "| Iteration Var"
              cons _    = ""
  show (Cons (l,c)) = "Constant value"
  show (EnumCons (l,c) sdt n ord) = "Enum "++sdt++" constant ("++show l++","++show c++ ") \'" ++ n ++ "\' with cardinaly " ++ show ord
  show (EmptyWithType t) = "(Forward Declaration of type "++ show t ++", this shouldn't be here)" 
  show Empty  = " EMPTY " 
  show (Enum   (l,c) n   scp ) = "Enum("++show l++","++show c++ ") "   
                                  ++ "\nType for variables: Enum "++ n 
                                  ++ "\nScope: " ++ showScope 1 scp ++ "\n"
  show (Union  (l,c) n t scp ) = "Union("++show l++","++show c++ ") "  
                                  ++ "\nType for variables: " ++ show n
                                  ++ "\nDeclare Type: " ++ show (TypeFunction t) 
                                  -- ++ "\nSize: "  ++ show s 
                                  ++ "\nScope: " ++ showScope 1 scp ++ "\n"
  show (Struct (l,c) n t scp ) = "Struct("++show l++","++show c++ ") " 
                                  ++ "\nType for variables: " ++ show n
                                  ++ "\nDeclare Type: " ++ show (TypeFunction t) 
                                  -- ++ "\nSize: "  ++ show s 
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
          | TypeString   
          | TypeFloat  
          | TypeVoid   
          | TypeEnumCons
          | TypeEnum       String -- Name comparison 
          | TypeStruct     { getDataName :: String} 
          | TypeUnion      { getDataName :: String}
          | TypeField      String Type
          | TypePointer    Type
          | TypeEmptyArray Type
          | TypeArray      Type Int
          | TypeFunction   { getTuple :: (Seq Type)}
          -- Helpers
          | TypeSatisfies  (Type -> Bool) 
          | TypeUndefined  -- Temporal
          | TypeError  

instance Eq Type where
  TypeString       ==   TypeString        = True
  TypeInt          ==   TypeInt           = True
  TypeEnumCons     ==   TypeEnumCons      = True
  TypeBool         ==   TypeBool          = True
  TypeChar         ==   TypeChar          = True
  TypeFloat        ==   TypeFloat         = True
  TypeVoid         ==   TypeVoid          = True
  TypeUndefined    ==   TypeUndefined     = True
  TypeError        ==   TypeError         = True
  TypeEnum       a ==   TypeEnum       b  = a==b
  TypeStruct     a ==   TypeStruct     b  = a==b
  TypeUnion      a ==   TypeUnion      b  = a==b
  TypePointer    a ==   TypePointer    b  = a==b
  TypeEmptyArray a ==   TypeEmptyArray b  = a==b
  TypeFunction   a ==   TypeFunction   b  = a==b
  TypeArray t1 d1  ==   TypeArray   t2 d2 = t1 == t2 && d1 == d2
  TypeSatisfies f  ==   TypeSatisfies g   = error "Cannot compare two TypeSatisfies, wtf is wrong with you?"
  TypeSatisfies f  ==   a                 = f a 
  a                ==   TypeSatisfies f   = f a 
  _  == _     = False


instance Show Type where
  show (TypeError    ) = "ERROR"
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
  show (TypeFunction    l     ) = "(" ++ (concat . intersperse "->" . (map show) . F.toList) l ++ ")"
  show blah = "dunno"


-- lists
nums = [TypeInt,TypeFloat]

structured :: Type -> Bool
structured (TypeStruct  _ ) = True
structured (TypeUnion  _ )  = True
structured _                = False

enumMatches :: Declare -> String -> Bool
enumMatches (Enum _ name _ ) str = name == str
enumMatches _ _                  = False

dataTypeMatches :: String  -> Type -> Bool
dataTypeMatches  str (TypeStruct name) = name == str
dataTypeMatches  str (TypeUnion name)  = name == str
dataTypeMatches _ _ = False

getFieldType :: Type -> Type
getFieldType (TypeField _ t) = t

dataNameMatches :: Declare -> String -> Bool
dataNameMatches (Struct _ (TypeStruct name) _ _ ) str = name == str
dataNameMatches (Union  _ (TypeUnion name)  _ _ ) str = name == str
dataNameMatches  Empty  _             = True
dataNameMatches _ _                    = False

isFunc :: Maybe Declare -> Bool
isFunc Nothing = False
isFunc (Just (Function _ _ _))  = True
isFunc (Just (EmptyWithType _)) = True -- Forward declaration
isFunc (Just Empty) = True
isFunc a = False

notErrors :: Type -> Type -> Type
notErrors a b = if (a /= TypeError && b /= TypeError) then TypeVoid
                                                      else TypeError


isEnumCons :: Declare -> Bool
isEnumCons (EnumCons  _ _ _ _) = True
isEnumCons _                     = False

isAFunction :: Declare -> Bool
isAFunction (Function _ _ _)  = True
isAFunction _ = False

--Check if a Declaration is an Iteration Variable
isLIter :: Declare -> Bool
isLIter (Variable _ _ iterVar _) = iterVar
isLIter _                        = False

--Check if a Declaration is a valid L-value
isLValue :: Type -> Declare -> Bool
--If it's a primType variable, return True
isLValue _ (Variable _ TypeInt   False _ ) = True
isLValue _ (Variable _ TypeBool  False _ ) = True
isLValue _ (Variable _ TypeChar  False _ ) = True
isLValue _ (Variable _ TypeFloat False _ ) = True
--TypeArray      Type Int
isLValue myT (Variable _ (TypePointer    varT)  False d ) = isLValue myT (Variable (0,0) varT  False d )
isLValue myT (Variable _ (TypeEmptyArray varT)  False d ) = isLValue myT (Variable (0,0) varT  False d )
isLValue myT (Variable _ (TypeArray    varT _)  False d ) = isLValue myT (Variable (0,0) varT  False d )

isLValue (TypeField _ TypeInt)   _       = True
isLValue (TypeField _ TypeBool)  _       = True
isLValue (TypeField _ TypeChar)  _       = True
isLValue (TypeField _ TypeFloat) _       = True
isLValue (TypeField _ (TypeField s myT)) rest =  isLValue (TypeField s myT) rest--Mismo caso
isLValue (TypeField _ (TypePointer myT)) rest =  isLValue (TypeField "s" myT) rest--Mismo caso
isLValue (TypeField _ (TypeEmptyArray myT)) rest =  isLValue (TypeEmptyArray myT) rest--Mismo caso
isLValue (TypeField _ (TypeArray myT i)) rest =  isLValue (TypeArray myT i) rest--Mismo caso
--enums
isLValue (TypeEnumCons) _     = False
isLValue (TypeEnum _) _       = True

isLValue (TypeEmptyArray TypeBool)  _                  = True
isLValue (TypeEmptyArray TypeChar)  _                  = True
isLValue (TypeEmptyArray TypeFloat) _                  = True
isLValue (TypeEmptyArray (TypePointer myT))   rest     = isLValue (TypePointer myT) rest
isLValue (TypeEmptyArray (TypeEmptyArray myT))   rest  = isLValue (TypeEmptyArray myT) rest
isLValue (TypeEmptyArray (TypeArray myT i))   rest     = isLValue (TypeArray myT i) rest
--isLValue (TypeEmptyArray TypeBool)  _       = True
--isLValue (TypeEmptyArray TypeChar)  _       = True
--isLValue (TypeEmptyArray TypeFloat) _       = True
--isLValue (TypeField _ TypePointer) --Mismo caso
isLValue _ _                                = False
  
--Checks if a declaration is readable
isReadable :: Maybe Declare -> Bool
isReadable (Just (Variable _ stType _ _)) = 
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
emptyTypeMatches (Function _ (TypeFunction t1) _)  t2 = t1 == t2
emptyTypeMatches Empty _ = True
emptyTypeMatches _ _     = False

{-
  TypeChecks
-}

isPointer :: Type -> Bool
isPointer (TypePointer _ ) = True
isPointer _                = False


isBasic :: Type -> Bool 
isBasic TypeInt    = True
isBasic TypeBool   = True
isBasic TypeChar   = True
isBasic TypeFloat  = True
isBasic _          = False

-- Type Sizes
getSize :: Type -> Int
getSize TypeInt          = 4  -- Basic types are going to change
getSize TypeBool         = 1
getSize TypeChar         = 1
getSize TypeFloat        = 4
getSize (TypeEnum _ )    = 4
getSize (TypePointer  _) = 4
 
getSize (TypeArray  t d) = d * getSize t

getSize (TypeStruct  _ ) = error "Cannot be calculated, get sum of scope" 
getSize (TypeUnion   _ ) = error "Cannot be calculated, get max of scope" 

getSize (TypeField  _ _) = error "Function as a type cannot be stored"
getSize (TypeEnumCons) = error "Enums are global"
getSize TypeVoid       = error "This type (void) cannot be stored"
getSize TypeString     = error "Global variable"
getSize (TypeSatisfies _ ) = error "wtf? really?"
-- size TypeEmptyArray = 4

-- Given a position, it returns the padding needed before insert and the new
-- offset
align :: Int -> (Int,Int)
align lastPos = if r == 0 then (lastPos      ,0)
                          else (lastPos+(4-r),4-r)
    where r = rem lastPos 4

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

transformType :: Type -> Type -> Type
transformType _ TypeError = TypeError
transformType _ a         = a

emptytuple :: TypeTuple 
emptytuple = empty

addType :: TypeTuple -> Type -> TypeTuple
addType = (|>)

funcReturnType :: TypeTuple -> Type
funcReturnType t = (decons . viewr) t
    where decons EmptyR = error "Empty sequence!"
          decons (others :> l) = l

-- addLeftType :: Type -> TypeTuple -> TypeTuple
-- addLeftType = (<|)

makeTypeTuple :: [Type] -> Type
makeTypeTuple = TypeFunction . fromList

-- Process two tuples, check if any pair of elements are not equal
-- and return last expected type, # of last argument processed and if it went ok
tuplesMatch :: TypeTuple -> TypeTuple -> (Type,Int,Bool)
tuplesMatch t1 t2  = F.foldl process (TypeVoid,1,True) tupSeq
    where tupSeq  = S.zipWith (,) t1 t2
          process b@(_,_,False) (_,_) = b
          process (_,field,ok) (TypeError,o)  = (o,field+1,True) -- Don't bother checking type errors
          process (_,field,ok) (o,TypeError)  = (o,field+1,True) 
          process (_,field,ok) (call,signature)  = newBase
              where newBase = if call == signature 
                                  then (call,field+1,True)
                                  else (signature,field,False)

lengthMatches :: TypeTuple -> TypeTuple -> Bool
lengthMatches t1 t2 = (S.length t1) == ((S.length t2) - 1)

-- singleType :: Type -> TypeTuple
-- singleType = Data.Sequence.empty |> 

isNotRecursiveData  :: String -> TypeTuple -> Bool
isNotRecursiveData s l = F.all (not .isRec) l
  where isRec item = dataTypeMatches s (getFieldType  item)