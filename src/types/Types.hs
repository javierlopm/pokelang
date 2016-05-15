module Types(
    Type(..),
    Declare(..),
    Message,
    makeDec
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

type Pos = (Int,Int)
type Message = Either String String -- Messages for monad writer

-- Las constantes enumeradas no deberian estar en un scope grande y universal?

-- Declarations might be functions,variables or structure types
data Declare = Function Pos Type (Scope Type)
             | Variable Pos Type
             | Enum     Pos -- Listas de constantes enumeradas No?
             | Struct   Pos (Scope Type)  
             | Union    Pos (Scope Type)  
             | Empty
             deriving(Show)

-- Types of Variable
data Type = TypeInt    Int
          | TypeFloat  Float
          | TypeChar   Char
          | TypeBool   Bool
          | TypeVoid   
          | TypeEnum  
          | TypeStruct  
          | TypeUnion  
          deriving(Show)
          -- | Pointer {type :: Type, levels     :: Int   }
          -- | Arr     {type :: Type, dimensions :: [Int] } -- Arreglos estaticos, los dinamicos son pointers

makeDec :: Token -> Declare
makeDec t = 
    case t of 
        TkInt    p -> Variable p (TypeInt 0) 
        TkBool   p -> Variable p (TypeBool False)
        TkChar   p -> Variable p (TypeChar '\0')
        TkFloat  p -> Variable p (TypeFloat 0.0)
        TkVoid   p -> Variable p TypeVoid
        -- TkStruct p -> Variable 
        -- TkUnion  p -> Variable 
        -- TkEnum   p -> Variable 

changePos :: Declare -> Pos -> Declate