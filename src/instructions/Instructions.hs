module Instructions(
    Ins(..),
    Operator(..),
    Exp(..),
    insertIns,
    newBlock,
    showIndented,
    goDipah
) where

-- import Data.Sequence(empty,viewl,length,Seq,(|>),(<|),ViewL((:<)),ViewR((:>)),(><))
import Data.Sequence(empty,Seq,(|>))
import Data.Foldable(toList)
import Data.List(intersperse)

data Ins = Assign    Exp Exp
         | AssignSum Exp Exp
         | AssignMin Exp Exp
         | AssignMul Exp Exp
         | Call  String (Seq(Exp))
         | If      { guards::Seq(Ins) }  -- Guards and else sequences
         | Guard   { cond  :: Exp } --, block:: Ins } Los bloques vienen dados por el anidamiento de scopes
         | Else    
         | While   { cond  :: Exp } 
         | ForStep { low:: Exp, high::Exp, step :: Exp }
         | For     { low:: Exp, high::Exp }
         | EnterFor
         | Read    { var::Exp }
         | Return  (Maybe Exp)
         | Continue
         | Break
         | Exit
         | Error
         | Block (Seq(Ins))

instance Show Ins where
  show (Assign    e1 e2 ) = show e1 ++ " = "  ++ show e2
  show (AssignSum e1 e2 ) = show e1 ++ " += " ++ show e2
  show (AssignMul e1 e2 ) = show e1 ++ " *= " ++ show e2
  show (AssignMin e1 e2 ) = show e1 ++ " *= " ++ show e2
  show (Call   s expSeq ) = show s ++ "(" ++ (( concat . (intersperse ",") . (map show) . toList) expSeq) ++ ")"
  show (Guard     bexp  ) = "If/Elif(" ++ show bexp ++ ")"
  -- Agregar while, for forStep
  show (Read      sexp  ) = "Read:" ++ show sexp
  show Else     = "Else:"
  show Continue = "Read:"
  show Break    = "Break"
  show Exit     = "Exit"
  show Error    = "Error"
  show _ = ""

showIndented :: Ins -> Int -> String
showIndented _ _  = undefined -- If, Block, for ... nested


insertIns :: Ins -> Ins -> Ins
insertIns ins (Block s)  = (Block (s |> ins) )
insertIns _   Error      = Error
insertIns _   bleh       = error ("Must insert in Block but " ++ show bleh ++ " found ")

newBlock :: Ins
newBlock = Block empty

goDipah :: Ins -> Bool
goDipah (Else          ) = True
goDipah (EnterFor      ) = True
goDipah (Guard _       ) = True 
goDipah (While _       ) = True
goDipah (For     _ _   ) = True
goDipah (ForStep _ _ _ ) = True
goDipah _ = False

data Operator = And -- Binary
              | Or 
              | SAnd 
              | SOr 
              | Div
              | FloatDiv
              | Neg 
              | Mod
              | Plus 
              | Minus
              | Multiply
              | FloatMultiply
              | Power
              | Greater
              | Less
              | LessEql
              | GreaterEql
              | NotEql
              -- Unary
              | Address -- Ampersand
              | Access  -- Arrays and structs
              | UNeg 
              | Not

instance Show Operator where
    show And           = "&&"
    show Or            = "||"
    show SAnd          = "and"
    show SOr           = "or"
    show Div           = "/"
    show FloatDiv      = "//"
    show Neg           = "-"
    show Mod           = "%"
    show Plus          = "+"
    show Minus         = "-"
    show Multiply      = "*"
    show FloatMultiply = "f*"
    show Power         = "^"
    show Greater       = ">"
    show Less          = "<"
    show LessEql       = "<="
    show GreaterEql    = ">="
    show NotEql        = "!="
    show Address      = "&"
    show Access        = "*"
    show UNeg          = "u-"
    show Not           = "!"


data Exp = Binary  Operator Exp Exp
         | Unary   Operator Exp
         | Value   String        -- Get Variable value
         | ExpTrue
         | ExpFalse
         | ExpFloat Float
         | ExpInt   Int
         | ExpEnum  String
         | CallVal  String (Seq(Exp)) -- Function call
         | NoExp
         deriving (Show)
