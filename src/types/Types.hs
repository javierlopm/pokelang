module Types(
    Type(..),
    Declare(..),
    Message,
    isPointer,
    makeDec,
    makePtrs
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
data Declare = Function     { pos::Pos , storedType::Type,fields::(Scope Type)}
             | Variable     { pos::Pos , storedType::Type }
             | Pointer      { pos::Pos , storedType::Type, levels    :: Int } -- No tiene mucho sentido que vengan con contenido...
             | StaticArray  { pos::Pos , storedType::Type, dimensions::[Int]}
             | Enum     Pos -- Listas de constantes enumeradas No?
             | Struct   Pos (Scope Type)  
             | Union    Pos (Scope Type)  
             | Empty
             -- | DynamicArray { pos::Pos , storedType::Type, } -- Azucar sintactica para aps
             deriving(Show)

-- Types of Variable
data Type = TypeInt    Int   -- Marcar con Maybe?
          | TypeFloat  Float
          | TypeChar   Char
          | TypeBool   Bool
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
makeDec :: Token -> Pos -> Declare
makeDec t p = 
    case t of 
        TkInt    _ -> Variable p (TypeInt 0) 
        TkBool   _ -> Variable p (TypeBool False)
        TkChar   _ -> Variable p (TypeChar '\0')
        TkFloat  _ -> Variable p (TypeFloat 0.0)
        TkVoid   _ -> Variable p TypeVoid
        -- TkStruct _ -> Variable 
        -- TkUnion  _ -> Variable 
        -- TkEnum   _ -> Variable 

-- Create a pointer declaration
makePtrs :: Token -> Pos -> Int -> Declare
makePtrs t p i = 
    case t of 
        TkInt    _ -> Pointer p (TypeInt 0)      i
        TkBool   _ -> Pointer p (TypeBool False) i
        TkChar   _ -> Pointer p (TypeChar '\0')  i 
        TkFloat  _ -> Pointer p (TypeFloat 0.0)  i 
        TkVoid   _ -> Pointer p  TypeVoid        i
  