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
import Data.Maybe(fromJust)

import Instructions hiding(Operator(Eql,NotEql,Mod,And,Or,Not))
import Tac          hiding(IntIns(Eql,NotEql,Mod,And,Or,Not))

import Instructions as I(Operator(Eql,NotEql,Mod,And,Or,Not))
import Tac          as T(IntIns(Eql,NotEql,Mod,And,Or,Not))

data TranlatorState  = TranlatorState { tempCount  :: Word
                                      , labelCount :: Word
                                      , trueLabel  :: Maybe Word
                                      , falseLabel :: Maybe Word }    
                                      --deriving(Show)      
type TreeTranslator  = StateT TranlatorState IO 

debugVar = True

vpp :: TranlatorState -> TranlatorState
vpp (TranlatorState w1 w2 w3 w4) = (TranlatorState (succ w1) w2 w3 w4)

alb :: TranlatorState -> TranlatorState
alb (TranlatorState w1 w2 w3 w4) = (TranlatorState w1 (succ w2) w3 w4)


initTranslator :: TranlatorState
initTranslator = TranlatorState 0 0 Nothing Nothing

newTemp :: TreeTranslator(Word)
newTemp = do nt <- gets tempCount
             modify vpp
             return nt

newLabel :: TreeTranslator(Word)
newLabel = do nl <- gets tempCount
              modify alb
              return nl

setJumps :: TreeTranslator (Word,Word)
setJumps = do lt <- newLabel
              lf <- newLabel
              modJumps True  (Just lt)
              modJumps False (Just lf)
              return (lt,lf)

unsetJumps :: TreeTranslator ()
unsetJumps = modJumps True Nothing >> modJumps False Nothing

getJumps :: TreeTranslator(Word,Word)
getJumps = do tl <- gets trueLabel
              fl <- gets falseLabel
              return (fromJust tl,fromJust fl)

swapNot :: TreeTranslator()
swapNot =

jumpsAreSet :: TreeTranslator (Bool)
jumpsAreSet = gets trueLabel >>= maybe (return False) ( \ _ -> return True)

modJumps :: Bool -> Maybe Word -> TreeTranslator()
modJumps True  mw = modify (\(TranlatorState a b _  c)->TranlatorState a b mw c)
modJumps False mw = modify (\(TranlatorState a b c _ )->TranlatorState a b c mw)

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
    (tac2,var2) <- expToTac e2
    (tac1,var1) <- expToTac e1

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
expToTac (ExpTrue    ) = return (empty,Int_Cons    1) -- gotoExpTrue
expToTac (ExpFalse   ) = return (empty,Int_Cons    0) -- gotoExpFalse
-- expToTac (Binary op (ExpInt i2) (ExpVar ev s)) = expToTac (Binary op (ExpVar ev s) (ExpInt i2))
  


-- expToTac node@(Binary I.And      exp1 exp2) = do
-- expToTac node@(Binary I.Or       exp1 exp2) = do


-- Field access in structs and unions
expToTac node@(Binary Access exp1 exp2) = do
    nt <- newTemp
    let newIns = maybe (ReadArray (Temp nt) Fp (Int_Cons ofs) ) 
                       (\ s -> ReadArray (Temp nt) (MemAdress s) (Int_Cons ofs)) 
                       addr
    return (empty |> newIns , Temp nt)
    where (addr,ofs) = getStructOrUnion node 


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
expToTac (Binary op exp1 exp2)
    | isCompare op = makeCompare exp1 exp2 op
    | otherwise    = do 
        (ins1,t1) <- expToTac exp1
        (ins2,t2) <- expToTac exp2
        nt <- newTemp
        return ((ins1 <> ins2) |> (insTranslation op (Temp nt) t1 t2) ,Temp nt)

expToTac (Unary op a) = do 
    tempLocal  <- newTemp
    (ins,wher) <- expToTac a
    return (ins |> (insTranslation' op wher (Temp tempLocal)),(Temp tempLocal))

expToTac _ = return (singleton Nop,Temp 0)

makeCompare :: Exp -> Exp -> Operator -> TreeTranslator ((Program,Var))
makeCompare exp1 exp2 oper = do 
    jAreSet <- jumpsAreSet
    if jAreSet 
        then do (p1,v1) <- expToTac exp1
                (p2,v2) <- expToTac exp2
                (lt,lf) <- getJumps
                -- Making counts for some other bool
                -- Second argument won't be need
                return ( (p1 <> p2) |> (mkOp oper v1 v2 lt) |> (Jump lf) , Fp ) 
        else do (lt,lf) <- setJumps
                (p1,v1) <- expToTac exp1
                (p2,v2) <- expToTac exp2
                nl      <- newLabel
                nt      <- newTemp
                -- could temp be in two places?
                let jumpCode = (mkRevOper oper v1 v2 lf) <| (jumpTrueFalse lt lf nl nt)
                unsetJumps
                return ( (p1 <> p2) <> jumpCode , (Temp nt) )


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
insTranslation Array      = ReadArray
--insTranslation Access     = ReadArray
-- insTranslation SAnd          = 
-- insTranslation SOr   =  

mkOp :: Operator -> Var -> Var -> Word -> IntIns
mkOp Greater    v1 v2 lab = (JGt  v1 v2 lab)
mkOp Less       v1 v2 lab = (JLt  v1 v2 lab)
mkOp LessEql    v1 v2 lab = (JLEq v1 v2 lab)
mkOp GreaterEql v1 v2 lab = (JGEq v1 v2 lab)
mkOp I.NotEql   v1 v2 lab = (JNEq v1 v2 lab)
mkOp I.Eql      v1 v2 lab = (JEq  v1 v2 lab)

mkRevOper :: Operator -> Var -> Var -> Word -> IntIns
mkRevOper Greater    v1 v2 lab = (JLEq v1 v2 lab)
mkRevOper Less       v1 v2 lab = (JGEq v1 v2 lab)
mkRevOper LessEql    v1 v2 lab = (JGt  v1 v2 lab)
mkRevOper GreaterEql v1 v2 lab = (JLt  v1 v2 lab)
mkRevOper I.NotEql   v1 v2 lab = (JEq  v1 v2 lab)
mkRevOper I.Eql      v1 v2 lab = (JNEq v1 v2 lab)

-- jumpingBool :: Operator -> Exp -> Exp -> Exp 

getStructOrUnion :: Exp -> (Maybe String,Int)
getStructOrUnion (Binary Access (ExpVar dec1 s1) (ExpVar dec2 s2)) = 
    case dir dec1 of 
        (Offset o) -> (Nothing, o + o2 )
        Label      -> (Just s1, o2)
  where (Offset o2) = dir dec2
getStructOrUnion (Binary Access exp1 (ExpVar dec s)) = (finalDir,totalofst + o)
    where (finalDir,totalofst) = getStructOrUnion exp1
          (Offset o) = dir dec
getStructOrUnion (Binary _ _ _) = error "Error at getStructOrUnion InsToTac.hs"


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

