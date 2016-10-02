module InsToTac(
    forestToTac,
    initTranslator,
    execTree,
    evalTree,
    TranlatorState(..),
    TreeTranslator
) where

import Data.Sequence(empty,Seq,(|>),(<|),(><))
import Data.Foldable(foldl)
import Data.Monoid((<>),mempty)
import Control.Applicative(pure)
import Data.Word(Word)
import Control.Monad.State

import Instructions hiding(Operator(Eql,NotEql,Div))
import Tac          hiding(IntIns(Eql,NotEql,Div))

import Instructions as I(Operator(Eql,NotEql,Div))
import Tac          as T(IntIns(Eql,NotEql,Div))

data TranlatorState  = TranlatorState { tempCount  :: Word
                                      , labelCount :: Word }    
                                      --deriving(Show)      
type TreeTranslator  = StateT TranlatorState IO 

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
    return ( pure (str,headTac) <> forestTacTail)


treeToTac :: Ins -> TreeTranslator (Program)
treeToTac (Assign e1 e2) = do 
    (tac1,var1) <- expToTac e1
    (tac2,var2) <- expToTac e2
    let finaltac = (tac1 <> tac2) <> (pure (Mv var1 var2))
    -- liftIO $ putStrLn $ show finaltac
    return finaltac

treeToTac (If    iS   ) = do 
    progSeq <- mapM treeToTac iS
    return $ foldl (><) empty progSeq
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
    progSeq <- mapM treeToTac iS
    return $ foldl (><) empty progSeq
treeToTac _ = return (pure Nop)

expToTac :: Exp -> TreeTranslator ((Program,Var))
expToTac (ExpInt   i1) = return (empty,Int_Cons   i1) -- Single constant values
expToTac (ExpFloat i1) = return (empty,Float_Cons i1)
-- Two integer constants
expToTac (Binary op (ExpInt i1) (ExpInt i2)) = do 
    let newVar = case op of 
                    Plus       ->  i1 + i2
                    Minus      ->  i1 - i2
                    Multiply   ->  i1 * i2
                    Mod        ->  i1 `mod` i2
                    I.Div      ->  i1 `div` i2
                    Power      ->  i1 ^ i2
                    I.Eql      ->  if i1 == i2 then 1 else 0
                    I.NotEql   ->  if i1 == i2 then 0 else 1
                    Less       ->  if i1 <  i2 then 1 else 0
                    LessEql    ->  if i1 <= i2 then 1 else 0
                    GreaterEql ->  if i1 >= i2 then 1 else 0
                    Greater    ->  if i1 >  i2 then 1 else 0
                    FloatDiv   ->  error "Trying to do float div over two integers"
    -- must check if its smaller than 16-bits threshold signed. Maybe leave this for mips
    if ((-32768) <= newVar) && (newVar <= 32767)
        then return (empty,Int_Cons newVar)
        else do nt <- newTemp
                return (empty |> (Mv (Temp nt) (Int_Cons newVar)) , (Temp nt))
expToTac (Binary op (ExpFloat i1) (ExpFloat i2)) = do -- Two integer constants
    let newVar = case op of 
                    Plus       -> i1 + i2
                    Minus      -> i1 - i2
                    Multiply   -> i1 * i2
                    FloatDiv   -> i1 / i2
                    I.Eql      -> if i1 == i2 then 1 else 0
                    I.NotEql   -> if i1 == i2 then 0 else 1
                    Less       -> if i1 <  i2 then 1 else 0
                    LessEql    -> if i1 <= i2 then 1 else 0
                    GreaterEql -> if i1 >= i2 then 1 else 0
                    Greater    -> if i1 >  i2 then 1 else 0
                    Power      -> error "Trying to do power two floats"
                    Mod        -> error "Trying to do mod over two floats"
    -- must check if its smaller than 16-bits threshold signed
    nt <- newTemp
    return (empty |> (Mv (Temp nt) (Float_Cons newVar)) , (Temp nt))
    
expToTac (Binary op exp1 exp2) = do 
    nt <- newTemp
    return (pure Nop,Temp nt)
expToTac _ = return (pure Nop,Temp 0)

-- Alias
execTree :: Monad m => StateT s m a -> s -> m s
execTree = execStateT

evalTree ::  Monad m => StateT s m a -> s -> m a
evalTree = evalStateT

