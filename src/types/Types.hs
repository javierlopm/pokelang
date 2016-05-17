module Types(
    Type(..),
    Declare(..),
    Message,
    isPointer,
    makeDec,
    makePtrs,
    makeType
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
                   ,TkNull))

type Message = Either String String -- Monad writer message unit
type Pos = (Int,Int)

-- Las constantes enumeradas no deberian estar en un scope grande y universal?

-- Declarations might be functions,variables or structure types
data Declare = Function     { pos::Pos , storedType::Type, fields::(Scope Declare)}
             | Variable     { pos::Pos , storedTypeV::PrimType }
             | Pointer      { pos::Pos , storedType::Type, levels    :: Int } -- No tiene mucho sentido que vengan con contenido...
             | StaticArray  { pos::Pos , storedType::Type, dimensions::[Int]}
             | Enum     Pos -- Listas de constantes enumeradas No?
             | Struct   Pos (Scope Type)  
             | Union    Pos (Scope Type)  
             | Empty
             -- | DynamicArray { pos::Pos , storedType::Type, } -- Azucar sintactica para aps
             deriving(Show)
data PrimType = PrimInt Int
              | PrimBool Bool
              | PrimChar  Char
              | PrimFloat  Float
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

-- Check if declare is a pointer
isPointer :: Declare -> Bool
isPointer (Pointer _ _ _ ) = True
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

-- Create a pointer declaration
makePtrs :: Token -> Pos -> Int -> Maybe Declare
makePtrs t p i = Just $
    case t of 
        TkInt    _ -> Pointer p (TypeInt)   i
        TkBool   _ -> Pointer p (TypeBool)  i
        TkChar   _ -> Pointer p (TypeChar)  i 
        TkFloat  _ -> Pointer p (TypeFloat) i 
        TkVoid   _ -> Pointer p  TypeVoid   i

makeType :: Token -> Type
makeType (TkInt   _) = TypeInt
makeType (TkBool  _) = TypeBool
makeType (TkChar  _) = TypeChar
makeType (TkVoid  _) = TypeVoid
makeType (TkFloat _) = TypeFloat