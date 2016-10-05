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

import Instructions hiding(Operator(Eql,NotEql,Mod,And,Or))
import Tac          hiding(IntIns(Eql,NotEql,Mod,And,Or))

import Instructions as I(Operator(Eql,NotEql,Mod,And,Or))
import Tac          as T(IntIns(Eql,NotEql,Mod,And,Or))

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
expToTac (ExpInt   i1) = return (empty,Int_Cons   i1) -- Single constant values
expToTac (ExpFloat i1) = return (empty,Float_Cons i1)
-- expToTac (ExpFloat i1) = return (empty,Float_Cons i1) Convertir true y false en 1 0
expToTac (Binary op (ExpInt i2) (ExpVar ev s)) = expToTac (Binary op (ExpVar ev s) (ExpInt i2))

-- Two integer constants
expToTac (Binary op (ExpInt i1) (ExpInt i2)) = do 
    let newVar = operate op i1 i2
    -- must check if its smaller than 16-bits threshold signed. Maybe leave this for mips
    if ((-32768) <= (getCons newVar)) && ( (getCons newVar) <= 32767)
        then return (empty, newVar)
        else do nt <- newTemp
                return (empty |> (Mv (Temp nt) newVar) , (Temp nt))

-- Two float constants
expToTac (Binary op (ExpFloat i1) (ExpFloat i2)) = do -- Two integer constants
    let newVar = operate op i1 i2
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

expToTac _ = return (singleton Nop,Temp 0)

loadLocal :: Int -> TreeTranslator((IntIns,Var))
loadLocal ofs = do tempLocal <- newTemp
                   let tl = (Temp tempLocal)
                   return ((ReadArray tl Fp (Int_Cons ofs)),tl)

operate :: Num a => constr -> a -> a -> Var
operate (Plusi     ) = ExpInt . (+)
operate (Minusi    ) = ExpInt . (-)
operate (Multiplyi ) = ExpInt . (*)
operate (I.Mod     ) = ExpInt . mod
operate (Divi      ) = ExpInt . div
operate (Power     ) = ExpInt . toInt . (^)
operate (I.Eql     ) = ExpInt . toInt . (==)
operate (I.NotEql  ) = ExpInt . toInt . (/=)
operate (Less      ) = ExpInt . toInt . (<)
operate (LessEql   ) = ExpInt . toInt . (<=)
operate (GreaterEql) = ExpInt . toInt . (>=)
operate (Greater   ) = ExpInt . toInt . (>)
operate (Plusf     ) = ExpFloat . (+)
operate (Minusf    ) = ExpFloat . (-)
operate (Multiplyf ) = ExpFloat . (*)
operate (Divf      ) = ExpFloat . (/)
-- operate I.And

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
-- insTranslation Address          = 
-- insTranslation Access          = 
-- insTranslation Not          = 
-- insTranslation Neg          = 
-- insTranslation SOr          = 
-- insTranslation SAnd          = 

-- insToOper :: Operator -> Var -> Var -> IntIns
-- insToOper Less () ()
-- insToOper Less () ()
-- insToOper Less () ()


toInt False = 0
toInt True  = 1


-- Alias
execTree :: Monad m => StateT s m a -> s -> m s
execTree = execStateT

evalTree ::  Monad m => StateT s m a -> s -> m a
evalTree = evalStateT

