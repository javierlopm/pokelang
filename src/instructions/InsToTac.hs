module InsToTac(
    forestToTac,
    initTranslator,
    execTree,
    evalTree,
    TranlatorState(..),
    TreeTranslator
) where

import Data.Sequence(empty,Seq,(|>),(<|),(><),singleton)
import qualified Data.Foldable as F(foldl)
import Data.Monoid((<>),mempty)
import Data.Word(Word)
import Control.Monad.State
import Types(Declare(..),Direction(..))
import qualified Data.Traversable as M(mapM)

import Instructions hiding(Operator(Eql,NotEql,Mod,And,Or,Not))
import Tac          hiding(IntIns(Eql,NotEql,Mod,And,Or,Not))

import Instructions as I(Operator(Eql,NotEql,Mod,And,Or,Not))
import Tac          as T(IntIns(Eql,NotEql,Mod,And,Or,Not))

data TranlatorState  = TranlatorState { tempCount  :: Word
                                      , labelCount :: Word }    
                                      --deriving(Show)      
type TreeTranslator  = StateT TranlatorState IO 

debugVar = True

vpp :: TranlatorState -> TranlatorState
vpp (TranlatorState w1 w2) = (TranlatorState (succ w1) w2)

initTranslator :: TranlatorState
initTranslator = TranlatorState 0 0

newTemp :: TreeTranslator(Word)
newTemp = do nt <- gets tempCount
             modify vpp
             return nt

-- use foldM instead
forestToTac :: [(String,Ins)] -> TreeTranslator ( [(String,Program)] )
forestToTac [] = return mempty
forestToTac ((str,insTree):tl)  = do 
    -- liftIO $ putStrLn $ show insTree
    headTac       <- treeToTac insTree
    forestTacTail <- forestToTac tl
    -- Maybe there'es no need to return, only write to file?
    return ( [(str,headTac)] <> forestTacTail)


treeToTac :: Ins -> TreeTranslator (Program)
treeToTac (Assign e1 e2) = do 
    (tac1,var1) <- expToTac e1
    (tac2,var2) <- expToTac e2
    let finaltac = (tac1 <> tac2) <> (singleton (StorePointer var1 var2))
    -- liftIO $ putStrLn $ show finaltac
    return finaltac

treeToTac (If    iS   ) = do 
    progSeq <- M.mapM treeToTac iS
    return $ F.foldl (><) empty progSeq
treeToTac (Else  ins ) = treeToTac ins >>= return
treeToTac (Guard cond ins) = do 
    insProg      <- treeToTac ins
    (condProg,_) <- expToTac cond
    return (condProg >< insProg) 
treeToTac (While cond ins   ) = do 
    insProg  <- treeToTac ins
    (condProg,_) <- expToTac cond
    return (condProg >< insProg)

treeToTac (For low high ins ) = do
    (lowProg ,_) <- expToTac low -- Maybe not needed, aren't they always constant numbers?
    (highProg,_) <- expToTac high
    insProg      <- treeToTac ins
    return $ (lowProg >< highProg) >< insProg
treeToTac (ForStep low high step ins ) = do
    (lowProg ,_) <- expToTac low -- Maybe not needed, aren't they always constant numbers?
    (highProg,_) <- expToTac high
    (stepProg,_) <- expToTac step
    insProg      <- treeToTac ins
    return $ (lowProg >< highProg) >< insProg

treeToTac (Block iS ) = do 
    progSeq <- M.mapM treeToTac iS
    return $ F.foldl (><) empty progSeq
treeToTac _ = return (singleton Nop)

expToTac :: Exp -> TreeTranslator ((Program,Var))
expToTac (Unary op (ExpInt   a) ) = return  (empty , operateui op a )
expToTac (Unary op (ExpFloat a) ) = return  (empty , operateuf op a )

expToTac (ExpInt   i1) = return (empty,Int_Cons   i1) -- Single constant values
expToTac (ExpFloat i1) = return (empty,Float_Cons i1)
expToTac (ExpTrue    ) = return (empty,Int_Cons    1)
expToTac (ExpFalse   ) = return (empty,Int_Cons    0)
expToTac (Binary op (ExpInt i2) (ExpVar ev s)) = expToTac (Binary op (ExpVar ev s) (ExpInt i2))

-- Two integer constants or booleans
expToTac (Binary op (ExpInt i1) (ExpInt i2)) = do 
    let newVar = operatei op i1 i2
    -- must check if its smaller than 16-bits threshold signed. Maybe leave this for mips
    if ((-32768) <= (getCons newVar)) && ( (getCons newVar) <= 32767)
        then return (empty, newVar)
        else do nt <- newTemp
                return (empty |> (Mv (Temp nt) newVar) , (Temp nt))

-- Two float constants
expToTac (Binary op (ExpFloat i1) (ExpFloat i2)) = do -- Two integer constants
    let newVar = operatef op i1 i2
    -- must check if its smaller than 16-bits threshold signed
    nt <- newTemp
    return (empty |> (Mv (Temp nt) newVar) , (Temp nt))

-- Single variable
expToTac (ExpVar dec s) = case (dir dec) of
        Label  -> return( commentedIns , (MemAdress s) )
        (Offset o) -> do 
            tempLocal <- newTemp
            let tl = Temp tempLocal
            return ( commentedIns |> (ReadArray tl Fp (Int_Cons o)) , tl )
    where commentedIns = if debugVar then empty |> (Comment ("Variable " ++ s)) else empty
-- Generic unkwon operation
expToTac (Binary op exp1 exp2) = do 
    nt <- newTemp
    (ins1,t1) <- expToTac exp1
    (ins1,t2) <- expToTac exp2
    return ( singleton (insTranslation op (Temp nt) t1 t2) ,Temp nt)
expToTac (Unary op a) = do 
    tempLocal  <- newTemp
    (ins,wher) <- expToTac a
    return (ins |> (insTranslation' op wher (Temp tempLocal)),(Temp tempLocal))

expToTac _ = return (singleton Nop,Temp 0)

loadLocal :: Int -> TreeTranslator((IntIns,Var))
loadLocal ofs = do tempLocal <- newTemp
                   let tl = (Temp tempLocal)
                   return ((ReadArray tl Fp (Int_Cons ofs)),tl)

operatei :: Operator -> Int -> Int -> Var
operatei Plusi      a b = Int_Cons  (a + b)
operatei Minusi     a b = Int_Cons  (a - b)
operatei Multiplyi  a b = Int_Cons  (a * b)
operatei Power      a b = Int_Cons  (a ^ b)
operatei I.Mod      a b = Int_Cons  (a `mod` b)
operatei Div        a b = Int_Cons  (a `div` b)
operatei I.Eql      a b = Int_Cons $ toInt $ a ==  b
operatei I.NotEql   a b = Int_Cons $ toInt $ a /=  b
operatei Less       a b = Int_Cons $ toInt $ a <   b
operatei LessEql    a b = Int_Cons $ toInt $ a <=  b
operatei GreaterEql a b = Int_Cons $ toInt $ a >=  b
operatei Greater    a b = Int_Cons $ toInt $ a >   b
operatei I.And      a b = Int_Cons $ toInt $ a == 1 && b == 1
operatei I.Or       a b = Int_Cons $ toInt $ a == 1 || b == 1

operatef :: Operator -> Float -> Float -> Var
operatef Plusf      a b = Float_Cons (a + b)
operatef Minusf     a b = Float_Cons (a - b)
operatef Multiplyf  a b = Float_Cons (a * b)
operatef FloatDiv   a b = Float_Cons (a / b)
operatef I.Eql      a b = Int_Cons $ toInt $ (a == b)
operatef I.NotEql   a b = Int_Cons $ toInt $ (a /= b)
operatef Less       a b = Int_Cons $ toInt $ (a <  b)
operatef LessEql    a b = Int_Cons $ toInt $ (a <= b)
operatef GreaterEql a b = Int_Cons $ toInt $ (a >= b)
operatef Greater    a b = Int_Cons $ toInt $ (a >  b)

operateui :: Operator -> Int -> Var
operateui Negi a   = Int_Cons (-a)
operateui I.Not 0  = Int_Cons  1
operateui I.Not _  = Int_Cons  0

operateuf :: Operator -> Float -> Var
operateuf Negf a = Float_Cons (-a) 

insTranslation :: Operator -> Var -> Var -> Var -> IntIns
insTranslation Greater    = Gt 
insTranslation Less       = Lt 
insTranslation LessEql    = LEq
insTranslation GreaterEql = GEq
insTranslation I.NotEql   = T.NotEql
insTranslation I.Eql      = T.Eql
insTranslation I.And      = T.And
insTranslation I.Or       = T.Or
insTranslation Div        = Divi 
insTranslation FloatDiv   = Divf
insTranslation Plusi      = Addi 
insTranslation Plusf      = Addf 
insTranslation Minusf     = Subf
insTranslation Minusi     = Subi
insTranslation I.Mod      = T.Mod
insTranslation Multiplyi  = Multi
insTranslation Multiplyf  = Multf
insTranslation Power      = Pot
insTranslation Access     = ReadArray
-- insTranslation SAnd          = 
-- insTranslation SOr   =  
    
insTranslation' :: Operator -> Var -> Var -> IntIns
insTranslation' I.Not   =  T.Not
insTranslation' Negi    =  Negaf
insTranslation' Negf    =  Negaf
insTranslation' Address =  ReadPointer

toInt :: Bool -> Int
toInt False = 0
toInt True  = 1

-- Alias
execTree :: Monad m => StateT s m a -> s -> m s
execTree = execStateT

evalTree ::  Monad m => StateT s m a -> s -> m a
evalTree = evalStateT

