module Types(
    Type(..),
    Declare(..),
    Message,
    isPointer,
    isReadable,
    isLIter,
    makeDec,
    makePtrs,
    makeType,
    makeArr,
    makeIter
) where

import TableTree(Scope(..))
import Tokens(Token(TkInt
                   ,TkBool
                   ,TkChar
                   ,TkVoid
                   ,TkFloat
                   ,TkStruct
                   ,TkUnion
                   ,TkEnum
                   ,TkNull
                   ,TkDId
                   ,TkId))

type Message = Either String String -- Monad writer message unit
type Pos = (Int,Int)

-- Las constantes enumeradas no deberian estar en un scope grande y universal?

-- Declarations might be functions,variables or structure types
data Declare = Function     { pos::Pos, 
                              storedType::Type, 
                              fields::(Scope Declare)}
             | Variable     { pos::Pos , storedTypeV::PrimType }
             | Cons         { pos::Pos }
             | Pointer      { pos::Pos ,
                              storedType:: Type,
                              levels    :: Int ,
                              dataID :: Maybe String }
             | StaticArray  { pos::Pos , 
                              storedType::Type, 
                              dimensions::[Integer]}
             | Enum     Pos -- Listas de constantes enumeradas No?
             | Struct   Pos (Scope Type)  
             | Union    Pos (Scope Type)  
             | Empty
             -- | DynamicArray { pos::Pos , storedType::Type, } -- Azucar sintactica para aps
             deriving(Show) -- Instance de Eq que ignore por para ver si ya algo esta en las glob

data PrimType = PrimInt     Int
              | PrimIter    Int
              | PrimBool    Bool
              | PrimChar    Char
              | PrimFloat   Float
              | PrimString  String
              | PrimEnum    String
              | PrimUnion   String
              | PrimStruct  String
              deriving(Show,Eq)

-- Types of Variable
data Type = TypeInt       -- Marcar con Maybe?
          | TypeFloat  
          | TypeChar   
          | TypeBool   
          | TypeVoid   
          | TypeEnum  
          | TypeStruct  
          | TypeUnion  
          deriving(Show,Eq)

isLIter :: Maybe Declare -> Bool
isLIter Nothing = False
isLIter (Just (Variable _ stType)) = case (stType) of
                                          PrimIter   _  -> True
                                          otherwise     -> False
isLIter a = False

--Checks if a declaration is readable
isReadable :: Maybe Declare -> Bool
isReadable Nothing = False
isReadable (Just (Variable _ stType)) = case (stType) of
                                          PrimInt    _  -> True
                                          PrimIter   _  -> True
                                          PrimBool   _  -> True
                                          PrimChar   _  -> True
                                          PrimFloat  _  -> True
                                          otherwise     -> False
isReadable a = False

-- Check if declare is a pointer
isPointer :: Declare -> Bool
isPointer (Pointer _ _ _ _) = True
isPointer _                = False

-- Create declare with a Token an a position
makeDec :: Token -> Pos -> Maybe String -> Maybe Declare
makeDec (TkVoid _) _ _ = Nothing
makeDec t p (Just s) = Just $
    case t of 
        TkStruct _ -> Variable p (PrimStruct s)
        TkUnion  _ -> Variable p (PrimUnion s)
        TkEnum   _ -> Variable p (PrimEnum s)
makeDec t p Nothing = Just $
    case t of 
        TkInt    _ -> Variable p (PrimInt 0) 
        TkBool   _ -> Variable p (PrimBool False)
        TkChar   _ -> Variable p (PrimChar '\0')
        TkFloat  _ -> Variable p (PrimFloat 0.0)

makeIter :: Token -> Pos -> Maybe Declare
makeIter (TkId _ _) p = Just $ Variable p (PrimIter 0) 
makeIter a p = Nothing

-- Create a pointer declaration
makePtrs :: Token -> Pos -> Int -> Maybe String -> Maybe Declare
makePtrs t p i s = Just $
    case t of 
        TkInt    _ -> Pointer p TypeInt    i Nothing
        TkBool   _ -> Pointer p TypeBool   i Nothing
        TkChar   _ -> Pointer p TypeChar   i Nothing
        TkFloat  _ -> Pointer p TypeFloat  i Nothing
        TkVoid   _ -> Pointer p TypeVoid   i Nothing
        TkStruct _ -> Pointer p TypeStruct i $ s
        TkUnion  _ -> Pointer p TypeUnion  i $ s
        TkEnum   _ -> Pointer p TypeEnum   i $ s

makeType :: Token -> Type
makeType (TkInt   _) = TypeInt
makeType (TkBool  _) = TypeBool
makeType (TkChar  _) = TypeChar
makeType (TkVoid  _) = TypeVoid
makeType (TkFloat _) = TypeFloat

-- Create a static array declaration
makeArr :: Token -> Pos -> [Integer] -> Maybe Declare
makeArr t p li = Just $
    case t of 
        TkInt    _ -> StaticArray p TypeInt    li
        TkBool   _ -> StaticArray p TypeBool   li
        TkChar   _ -> StaticArray p TypeChar   li
        TkFloat  _ -> StaticArray p TypeFloat  li
        TkVoid   _ -> StaticArray p TypeVoid   li
        --TkStruct _ -> StaticArray p TypeStruct i $ s
        --TkUnion  _ -> StaticArray p TypeUnion  i $ s
        --TkEnum   _ -> StaticArray p TypeEnum   i $ s