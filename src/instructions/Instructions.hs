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
    addExpList,
    stripBlock,
    printAsts,
    isCompare,
    isAO,
    brokenChain,
    itsVar
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
         | Call      String (Seq(Exp)) [Int] Bool
         | If      { guards::Seq(Ins) }
         | Guard   { cond  :: Exp, ins :: Ins }
         | Else    { ins   :: Ins }
         | While   { cond  :: Exp, ins :: Ins } 
         | ForStep { low:: Ins, high::Exp, step :: Exp, ins :: Ins }
         | For     { low:: Ins, high::Exp, ins :: Ins }
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
showIndented n  (Call   s expSeq i False) = "FunctionCall: " ++ ind n ++ show s ++ "(" ++ (( concat . (intersperse ",\n") . (map show) . toList) expSeq) ++ ")" ++ show i ++ "bytes"
showIndented n  (Call   s expSeq i True) = "ProcedureCall: " ++ ind n ++ show s ++ "(" ++ (( concat . (intersperse ",\n") . (map show) . toList) expSeq) ++ ")" ++ show i ++ "bytes"
showIndented n  (Guard  bexp ins ) = ind n ++ "If/Elif(" ++ show bexp ++ "):" ++ showIndented (n+1) ins
showIndented n  (While  bexp ins ) = ind n ++ "While("   ++ show bexp ++ "):" ++ showIndented (n+1) ins
showIndented n  (Else   ins   )    = ind n ++ "Else:" ++ showIndented (n+1) ins
showIndented n  (For     low high   ins ) = ind n ++  "For" ++ show low ++ " to " ++  show high ++ ":" ++ showIndented (n+1) ins
showIndented n  (ForStep low high w ins ) = ind n ++  "For" ++ show low ++ " to " ++  show high ++ " with  " ++ show w ++ ":" ++ showIndented (n+1) ins
showIndented n  (Read      sexp  ) = ind n ++ "Read:"     ++ show sexp
showIndented n  (Return  Nothing ) = ind n ++ "Return Void"
showIndented n  (Return  (Just exp)) = ind n ++  "Return " ++ show exp
showIndented n  (If guardSeq ) = concat $ map (showIndented n) (toList guardSeq)
showIndented n  Continue = ind n ++ "Continue"
showIndented n  Break    = ind n ++ "Break"
showIndented n  Exit     = ind n ++ "Exit"
showIndented n  (Block ins) = ( concat . (intersperse ",\n") . (map (showIndented n) ) . toList) ins
showIndented n  Error    = "\nError"
showIndented n  _ = "DefaultIns"

printAsts :: [(String,Ins,Int)] -> String
printAsts = concatMap beautyprint 
    where beautyprint (str,blck,int) = str ++ "\n--------------" ++ show blck ++ "\n\n"

-- shortcut for indentation
ind :: Int -> String
ind n = "\n" ++ replicate (n*4) ' '

insertIns :: Ins -> Ins -> Ins
insertIns ins (Block s)  = (Block (s |> ins) )
insertIns _   Error      = Error
insertIns _   bleh       = error ("Must insert in Block but " ++ show bleh ++ " found ")

stripBlock :: Ins -> Seq( Ins )
stripBlock (Block seqins) = seqins
stripBlock a = error "Compile error. Trying to strip nonBlock instruction"

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
              | Mod
              | Array
              | Negi 
              | Negf 
              | Plusi 
              | Plusf 
              | Minusi
              | Minusf
              | Multiplyi
              | Multiplyf
              -- | FloatMultiply
              | Power
              | Greater
              | Less
              | LessEql
              | GreaterEql
              | NotEql
              | Eql
              -- Unary
              | Address -- Ampersand
              | Access  --  structs
              | Not

instance Show Operator where
    show And           = "&&"
    show Or            = "||"
    show SAnd          = "and"
    show SOr           = "or"
    show Div           = "/"
    show FloatDiv      = "//"
    show Mod           = "%"
    show Negi           = "-"
    show Negf           = "f-"
    show Plusi          = "+"
    show Plusf          = "f+"
    show Minusi         = "-"
    show Minusf         = "f-"
    show Multiplyi      = "*"
    show Multiplyf      = "f*"
    show Array          = "Aray"
    --show FloatMultiply = "f*"
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
         | ExpVar  { dec::Declare , nam::String }
         | ExpTrue
         | ExpFalse
         | ExpFloat Float
         | ExpInt   Int
         | ExpChar  Char
         | ExpEnum  String
         | CallVal  String (Seq(Exp)) [Int] Bool
         | NoExp
         deriving (Show)

isCompare :: Operator -> Bool
isCompare Greater    = True
isCompare Less       = True
isCompare LessEql    = True
isCompare GreaterEql = True
isCompare NotEql     = True
isCompare Eql        = True
isCompare _          = False

isAO :: Operator -> Bool
isAO And = True
isAO Or  = True
isAO _   = False

itsVar :: Exp -> Bool
itsVar (ExpVar _ _) = True
itsVar _ = False 

brokenChain :: Operator -> Exp -> Bool
brokenChain And (Binary Or  _ _) = False
brokenChain Or  (Binary And _ _) = False
brokenChain _  _                 = True

emptyExpList :: (Seq(Exp))
emptyExpList = empty

addExpList :: (Seq(Exp)) -> Exp -> (Seq(Exp))
addExpList = (|>)
