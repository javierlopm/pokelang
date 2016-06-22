module Instructions(
    Ins(..),
    Operator(..),
    Exp(..),
    insertIns,
    newBlock,
    newIf,
    goDipah,
    mergeIf,
    insertIf,
    emptyExpList,
    addExpList
) where

-- import Data.Sequence(empty,viewl,length,Seq,(|>),(<|),ViewL((:<)),ViewR((:>)),(><))
import Data.Sequence(empty,Seq,(|>),(<|))
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
         | EnterBlock
         | Block (Seq(Ins))

instance Show Ins where
  show (Assign    e1 e2 ) = show e1 ++ " = "  ++ show e2
  show (AssignSum e1 e2 ) = show e1 ++ " += " ++ show e2
  show (AssignMul e1 e2 ) = show e1 ++ " *= " ++ show e2
  show (AssignMin e1 e2 ) = show e1 ++ " *= " ++ show e2
  show (Call   s expSeq ) = show s ++ "(" ++ (( concat . (intersperse ",") . (map show) . toList) expSeq) ++ ")"
  show (Guard     bexp  ) = "If/Elif(" ++ show bexp ++ "):"
  show (While     bexp  ) = "While(" ++ show bexp ++ "):"
  show (For     low high ) = "For" ++ show low ++ " to " ++  show high ++ ":"
  show (ForStep low high w ) = "For" ++ show low ++ " to " ++  show high ++ " with  " ++ show w ++ ":"
  show (Read      sexp  ) = "Read:" ++ show sexp
  show (Return  Nothing ) = "Return Void"
  show (Return  (Just exp)) = "Return " ++ show exp
  show EnterBlock = "Block:"
  show Else     = "Else:"
  show Continue = "Read:"
  show Break    = "Break"
  show Exit     = "Exit"
  show Error    = "Error"
  show _ = "DefaultIns"

--showIndented :: Int -> Ins -> String
--showIndented n  (Assign    e1 e2 ) = showIndExp (n+1) e1 ++ " = "  ++ show (n+1) e2
--showIndented n  (AssignSum e1 e2 ) = showIndExp (n+1) e1 ++ " += " ++ show (n+1) e2
--showIndented n  (AssignMul e1 e2 ) = showIndExp (n+1) e1 ++ " *= " ++ show (n+1) e2
--showIndented n  (AssignMin e1 e2 ) = showIndExp (n+1) e1 ++ " *= " ++ show (n+1) e2
--showIndented n  (Call   s expSeq ) = show s ++ "(" ++ (( concat . (intersperse ",\n") . (map (showIndented n)) . toList) expSeq) ++ ")"
--showIndented n  (Guard     bexp  ) = "If/Elif(" ++ showIndExp (n+1) bexp ++ "):"
--showIndented n  (While     bexp  ) = "While("   ++ showIndExp (n+1) bexp ++ "):"
--showIndented n  (For     low high )   = "For" ++ showIndExp (n+1) low ++ " to " ++  showIndExp (n+1) high ++ ":"
--showIndented n  (ForStep low high w ) = "For" ++ showIndExp (n+1) low ++ " to " ++  showIndExp (n+1) high ++ " with  " ++ showIndExp (n+1) w ++ ":"
--showIndented n  (Read      sexp  ) = "Read:"     ++ showIndExp (n+1) sexp
--showIndented n  (Return  Nothing ) = "Return Void"
--showIndented n  (Return  (Just exp)) = "Return " ++ showIndExp (n+1) exp
--showIndented n  EnterBlock = "Block:"
--showIndented n  Else     = "Else:"
--showIndented n  Continue = "Read:"
--showIndented n  Break    = "Break"
--showIndented n  Exit     = "Exit"
--showIndented n  Error    = "Error"
--showIndented n  _ = "DefaultIns"



insertIns :: Ins -> Ins -> Ins
insertIns ins (Block s)  = (Block (s |> ins) )
insertIns _   Error      = Error
insertIns _   bleh       = error ("Must insert in Block but " ++ show bleh ++ " found ")

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

goDipah :: Ins -> Bool
goDipah (Else          ) = True
goDipah (EnterFor      ) = True
goDipah (EnterBlock    ) = True
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