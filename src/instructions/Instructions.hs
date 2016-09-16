module Instructions(
    Ins(..),
    Operator(..),
    Exp(..),
    insertIns,
    newBlock,
    newIf,
    mergeIf,
    insertIf,
    emptyExpList,
    addExpList
) where

-- import Data.Sequence(empty,viewl,length,Seq,(|>),(<|),ViewL((:<)),ViewR((:>)),(><))
import Data.Sequence(empty,Seq,(|>),(<|))
import Data.Foldable(toList)
import Data.List(intersperse)
import Data.Tree
import Types(Declare(..),Direction(..),Type(..))




data Ins = Assign    Exp Exp
         | AssignSum Exp Exp
         | AssignMin Exp Exp
         | AssignMul Exp Exp
         | Call      String (Seq(Exp))
         | If      { guards::Seq(Ins) }
         | Guard   { cond  :: Exp, ins :: Ins }
         | Else    { ins   :: Ins }
         | While   { cond  :: Exp, ins :: Ins } 
         | ForStep { low:: Exp, high::Exp, step :: Exp, ins :: Ins }
         | For     { low:: Exp, high::Exp, ins :: Ins }
         | Read    { var::Exp }
         | NoOp
         | Return  (Maybe Exp)
         | Continue
         | Break
         | Exit
         | Error
         | Block (Seq(Ins))


instance Show Ins where
  show ins = showIndented 0 ins

showIndented :: Int -> Ins -> String
showIndented n  (Assign    e1 e2 ) = ind n ++ show e1 ++ " = "  ++ show e2 
showIndented n  (AssignSum e1 e2 ) = ind n ++ show e1 ++ " += " ++ show e2 
showIndented n  (AssignMul e1 e2 ) = ind n ++ show e1 ++ " *= " ++ show e2 
showIndented n  (AssignMin e1 e2 ) = ind n ++ show e1 ++ " *= " ++ show e2 
showIndented n  (Call   s expSeq ) = ind n ++ show s ++ "(" ++ (( concat . (intersperse ",\n") . (map show) . toList) expSeq) ++ ")"
showIndented n  (Guard  bexp ins ) = ind n ++ "If/Elif(" ++ show bexp ++ "):\n" ++ showIndented (n+1) ins
showIndented n  (While  bexp ins ) = ind n ++ "While("   ++ show bexp ++ "):\n" ++ showIndented (n+1) ins
showIndented n  (Else   ins   )    = ind n ++ "Else:\n" ++ showIndented (n+1) ins
showIndented n  (For     low high   ins ) = ind n ++  "For" ++ show low ++ " to " ++  show high ++ ":\n" ++ showIndented (n+1) ins
showIndented n  (ForStep low high w ins ) = ind n ++  "For" ++ show low ++ " to " ++  show high ++ " with  " ++ show w ++ ":\n" ++ showIndented (n+1) ins
showIndented n  (Read      sexp  ) = ind n ++ "Read:\n"     ++ show sexp
showIndented n  (Return  Nothing ) = ind n ++ "Return Void\n"
showIndented n  (Return  (Just exp)) = ind n ++  "Return " ++ show exp
showIndented n  Continue = ind n ++ "Continue"
showIndented n  Break    = ind n ++ "Break"
showIndented n  Exit     = ind n ++ "Exit"
showIndented n  (Block ins) = ( concat . (intersperse ",\n") . (map (showIndented n) ) . toList) ins
showIndented n  Error    = "Error"
showIndented n  _ = "DefaultIns"

-- shortcut for indentation
ind :: Int -> String
ind n =  replicate (n*4) ' '

insertIns :: Ins -> Ins -> Ins
insertIns ins (Block s)  = (Block (s |> ins) )
insertIns _   Error      = Error
insertIns _   bleh       = error ("Must insert in Block but " ++ show bleh ++ " found ")

-- Given a If instruction, a Elseif sequence and a Else instructions
-- Merges all instructions in one if
mergeIf :: Ins -> Ins -> Maybe Ins -> Ins
mergeIf Error _ _ = Error
mergeIf _ Error _ = Error
mergeIf _ _ (Just Error) = Error
mergeIf iif (If elifs) elsei = If $ maybe headIns (headIns |> ) elsei
    where headIns = (iif <| elifs)


newBlock :: Ins
newBlock = Block empty

newIf :: Ins
newIf = If empty

insertIf :: Ins -> Ins -> Ins 
insertIf (If ifs) guard = (If (ifs |> guard))

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
              | Eql
              -- Unary
              | Address -- Ampersand
              | Access  -- Arrays and structs
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
    show Eql           = "=="
    show Address       = "&"
    show Access        = "->"
    show Not           = "!"


data Exp = Binary  Operator Exp Exp
         | Unary   Operator Exp
         | ExpVar  String        
         | ExpTrue
         | ExpFalse
         | ExpFloat Float
         | ExpInt   Int
         | ExpChar  Char
         | ExpEnum  String
         | CallVal  String (Seq(Exp)) 
         | NoExp
         deriving (Show)

--showIndExp :: Int -> Exp -> String
--showIndExp n (Binary  op e1 e2)      =  show op 
--showIndExp n (Unary   op e1   )      =
--showIndExp n (ExpVar  String)              =
--showIndExp n (ExpTrue)                     =
--showIndExp n (ExpFalse)                    =
--showIndExp n (ExpFloat Float)              =
--showIndExp n (ExpInt   Int)                =
--showIndExp n (ExpChar  Char)               =
--showIndExp n (ExpEnum  String)             =
--showIndExp n (CallVal  String (Seq(Exp)) ) =
--showIndExp n (NoExp)                       =


emptyExpList :: (Seq(Exp))
emptyExpList = empty

addExpList :: (Seq(Exp)) -> Exp -> (Seq(Exp))
addExpList = (|>)