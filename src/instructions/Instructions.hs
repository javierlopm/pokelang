module Instructions(
    Ins(..),
    Operator(..),
    Exp(..),
    insertIns,
    newBlock
) where

-- import Data.Sequence(empty,viewl,length,Seq,(|>),(<|),ViewL((:<)),ViewR((:>)),(><))
import Data.Sequence(empty,Seq,(|>))

data Ins = Assign    String Exp
         | AssignSum String Exp
         | AssignMin String Exp
         | AssignMUl String Exp
         | Call  String (Seq(Exp))
         | If      { guards::Seq(Ins) }  -- Guards and else sequences
         | Guard   { cond  :: Exp} --, block:: Ins } Los bloques vienen dados por el anidamiento de scopes
         | Else    
         | While   { cond  :: Exp} --, block:: Ins} scope
         | ForStep { low:: Exp, high::Exp, step :: Exp} --, block:: Ins} scope
         | For     { low:: Exp, high::Exp} --, block:: Ins} scope
         | EnterFor
         | Read    { var::Exp} --, block:: Ins} scope
         | Return  (Maybe Exp)
         | Continue
         | Break
         | Exit
         | Error
         | Block (Seq(Ins))
    deriving (Show)

insertIns :: Ins -> Ins -> Ins
insertIns (Block s) ins = (Block (s |> ins) )
insertIns Error     _   = Error
ins       bleh      _   = error "Must insert in Block but " ++ show bleh ++ " found "

newBlock :: Ins
newBlock = Block empty

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

--data Position = Label  String
--              | Offset Int
--              deriving(Show)

data Exp = Binary  Operator Exp Exp
         | Unary   Operator Exp
         | Value   String        -- Get Variable value
         | ExpTrue
         | ExpFalse
         | ExpFloat Float
         | ExpInt   Int
         | ExpEnum  String
         | CallVal  String (Seq(Exp)) -- Function call
         deriving (Show)
