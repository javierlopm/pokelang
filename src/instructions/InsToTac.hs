module InsToTac(
    forestToTac,
    initTranslator,
    forestToTac',
    execTree,
    evalTree,
    translateStrings,
    TranlatorState(..),
    TreeTranslator
) where

import Data.Sequence(empty,Seq,(|>),(<|),(><),viewl,ViewL(..),viewr,ViewR(..),singleton)
import qualified Data.Sequence as S(null)
import qualified Data.Foldable as F(foldl,toList)
import Data.Monoid((<>),mempty)
import Data.Word(Word)
import Control.Monad.State
import Types(Declare(..),Direction(..))
import qualified Data.Traversable as M(mapM)
import Data.Maybe(fromJust)
import Types
import Instructions hiding(Operator(Eql,NotEql,Mod,And,Or,Not))
import Tac          hiding(IntIns(Eql,NotEql,Mod,And,Or,Not))

import Instructions as I(Operator(Eql,NotEql,Mod,And,Or,Not))
import Tac          as T(IntIns(Eql,NotEql,Mod,And,Or,Not))

translateStrings :: [Declare] -> Program 
translateStrings  = foldl addDec empty
    where addDec prog (StrCons _ (ThisLab l) val) = prog |> (TagSC l val)

data TranlatorState  = TranlatorState { tempCount  :: Word       -- Temporal variables generator
                                      , labelCount :: Word       -- Label generator
                                      , trueLabel  :: Maybe Word -- Last true label created
                                      , falseLabel :: Maybe Word -- Last false label created
                                      , beginLabel :: Word       -- to what label the jump was made
                                      , endLabel   :: Word       -- jump if true or false found
                                      , isItLval   :: Bool}
                                      --deriving(Show)      
type TreeTranslator  = StateT TranlatorState IO 

debugVar = True

vpp :: TranlatorState -> TranlatorState
vpp (TranlatorState w1 w2 w3 w4 b w5 l) = (TranlatorState (succ w1) w2 w3 w4 b w5 l)

alb :: TranlatorState -> TranlatorState
alb (TranlatorState w1 w2 w3 w4 b w5 l) = (TranlatorState w1 (succ w2) w3 w4 b w5 l)


initTranslator :: TranlatorState
initTranslator = TranlatorState 0 0 Nothing Nothing 0 0 False

newTemp :: TreeTranslator(Word)
newTemp = do nt <- gets tempCount
             modify vpp
             return nt

newLabel :: TreeTranslator(Word)
newLabel = do nl <- gets labelCount
              modify alb
              return nl

setJumps :: TreeTranslator (Word,Word)
setJumps = do lt <- newLabel
              lf <- newLabel
              modJumps True  (Just lt)
              modJumps False (Just lf)
              return (lt,lf)

setTheseJumps :: Word -> Word -> TreeTranslator ()
setTheseJumps lt lf = modJumps True  (Just lt) >> modJumps False (Just lf)

unsetJumps :: TreeTranslator ()
unsetJumps = modJumps True Nothing >> modJumps False Nothing

getJumps :: TreeTranslator(Word,Word)
getJumps = do tl <- gets trueLabel
              fl <- gets falseLabel
              return (fromJust tl,fromJust fl)

getBegEnd :: TreeTranslator(Word,Word)
getBegEnd = do tl <- gets beginLabel
               fl <- gets endLabel
               return (tl, fl)

isLval :: TreeTranslator (Bool)
isLval = gets isItLval >>= return

setLval :: TreeTranslator ()
setLval = modify (\(TranlatorState w1 w2 tl fl d w3 _) -> (TranlatorState w1 w2 fl tl d w3 True))

setRval :: TreeTranslator ()
setRval = modify (\(TranlatorState w1 w2 tl fl d w3 _) -> (TranlatorState w1 w2 fl tl d w3 False))

swapNot :: TreeTranslator()
swapNot = modify (\(TranlatorState w1 w2 tl fl d w3 l) -> (TranlatorState w1 w2 fl tl d w3 l))

setBegin :: Word -> TreeTranslator()
setBegin bl = modify (\(TranlatorState w1 w2 tl fl _ w3 l) -> (TranlatorState w1 w2 tl fl bl w3 l))

setEnd :: Word -> TreeTranslator()
setEnd el = modify (\(TranlatorState w1 w2 tl fl b _ l) -> (TranlatorState w1 w2 tl fl b el l))

jumpsAreSet :: TreeTranslator (Bool)
jumpsAreSet = gets trueLabel >>= maybe (return False) ( \ _ -> return True)

modJumps :: Bool -> Maybe Word -> TreeTranslator()
modJumps True  mw = modify (\(TranlatorState a b _  c d e f)->TranlatorState a b mw c d e f)
modJumps False mw = modify (\(TranlatorState a b c _  d e f)->TranlatorState a b c mw d e f)

--totalType :: TypeTuple -> Int
--totalType a = (sum (foldl getSize 0 (F.toList a))) - (getSize $ funcReturnType a )
-- use foldM instead
forestToTac' :: [(String,Ins,TypeTuple)] -> TreeTranslator ( [(String,Program)] )
forestToTac' a = mapM buildFun  a
    where dr1 (a,b,c) = (a,b)
          buildFun a@(str,ins,typ) = do  ((str,prg):_) <- forestToTac [dr1 a]
                                         if str == "hitMAINlee" 
                                            then  return (str, (prg |> TacExit))
                                            else  return (str, ((singleton (TagS str)) |> (TACCall "Prologue" 42) )<> prg |> (TagS (str++"_epilogue"))   |> (TACCall "Epilogue" 42) )
                                         --return (str, ((singleton (TagS str)) |> (TACCall "Prologue" 42) )<> prg |> (TACCall "Epilogue" 42))
forestToTac :: [(String,Ins)] -> TreeTranslator ( [(String,Program)] )
forestToTac [] = return mempty
forestToTac ((str,insTree):tl)  = do 
    -- liftIO $ putStrLn $ show insTree
    headTac       <- treeToTac insTree
    forestTacTail <- forestToTac tl
    tuple         <- functionsToTac (str,headTac)
    --liftIO $ putStrLn $ "\nAAAAAAAAAAAAAAAA---------------\n" ++ show insTree ++"AAAAAAAAAAAAAAAAAAA---------------\n"
    -- Maybe there'es no need to return, only write to file?
    return ( [tuple] <> forestTacTail)

functionsToTac :: (String,Program) -> TreeTranslator ( (String,Program) )
functionsToTac ("hitMAINlee",prg) = return ("hitMAINlee",prg)
functionsToTac (str,prg) = return (str,prg)

treeToTac :: Ins -> TreeTranslator (Program)
treeToTac (Assign e1 e2) = do
    setLval
    (tac2,var2) <- expToTac e2
    setRval
    (tac1,var1) <- expToTac e1

    let finaltac = (tac1 <> tac2) <> (singleton (StorePointer var1 var2))
    -- liftIO $ putStrLn $ show finaltac
    
    return finaltac
treeToTac (If    iS   ) = do
    -- liftIO $ putStrLn $ show iS
    ending <- newLabel
    ifcode <- foldM  processGuard empty (F.toList iS)
    return (ifcode |> (Tag ending))
  where processGuard accCode (Guard exp1 ins) = do 
            (lt,lf)       <- setJumps

            (guardCode,_) <- expToTac exp1
            unsetJumps
            blockCode     <- treeToTac ins
            let lastIf = (accCode <> guardCode |> (Tag lt)) <> blockCode |> (Comment "end") |> (Tag lf)
            return lastIf 

        processGuard accCode (Else ins) = treeToTac ins >>= return . (accCode <>)
        processGuard _       a  = error $ "errror" ++ show a

treeToTac (While cond ins   ) = do 
    (oldb,olde) <-getBegEnd
    begl <- newLabel
    endl <- newLabel
    setBegin begl
    setEnd   endl
    (lt,lf)<- setJumps

    insProg  <- treeToTac ins
    (condProg,_) <- expToTac cond

    -- In case of nested whiles, needed for break and continue
    unsetJumps
    setBegin oldb
    setEnd   olde

    return ((((empty |> (Tag begl)) <> condProg |> (Tag lt)) <> insProg) |> (Jump begl) |> (Tag lf))

treeToTac (For low high ins ) = treeToTac (ForStep low high (ExpInt 1) ins)
    
treeToTac (ForStep low high step ins ) = do
    (oldb,olde) <-getBegEnd
    begl <- newLabel
    endl <- newLabel

    lowProg <- treeToTac low -- Maybe not needed, aren't they always constant numbers?
    (highProg,hvar) <- expToTac high
    insProg      <- treeToTac ins

    (prog_step,step_var) <- expToTac high
    -- In case of nested fors, needed for break and continue
    unsetJumps
    setBegin oldb
    setEnd   olde

    
    inc   <- treeToTac (Assign (left low) (Binary Plusi (left low) step))

    (mkIter,iter) <- expToTac (left low)

    return $ (lowProg |> (Tag begl))<>insProg<>highProg<>prog_step <> inc <> mkIter |> (JNEq iter hvar begl) |> (Tag endl)

  where left (Assign a b) = a

treeToTac (Block iS ) = do 
    progSeq <- M.mapM treeToTac iS
    return $ F.foldl (><) empty progSeq
treeToTac (Call s args size b) = do
                            prog <- argsToProg args b empty 
                            return $ prog <> singleton (TACCall s size)

treeToTac (Return v)  = do
    maybe ( return (singleton (ReturnE) ))
          ( \nVar -> do 
            (retProg,rVar) <- expToTac nVar
            let fTac = (retProg <>  (singleton (ReturnS rVar) ) )
            return (fTac))
          (v)
treeToTac (Read e1) = do isL <- isLval
                         let goback = if isL then return () else setRval
                         setLval
                         (var_cal,var) <- expToTac e1
                         goback
                         return $ var_cal |> (Param var) |> (TACCall "read" 1)
treeToTac _ = return (singleton Nop)
 
argsToProg :: (Seq(Exp)) -> Bool ->  Program ->  TreeTranslator((Program))
argsToProg s b s0 =  if (S.null s)
                   then do
                        return s0
                   else do
                    let firstExp =  (fst . decons . viewl) s
                    if b then
                        do
                          setLval
                          (argProg,rArg) <- expToTac firstExp
                          setRval
                          let pamToAdd = singleton (Param rArg)
                          argsToProg ( (snd . decons . viewl) s) b $ s0 <> argProg <> pamToAdd
                    else
                        do
                          (argProg,rArg) <- expToTac firstExp
                          argsToProg ( (snd . decons . viewl) s) b $ s0 <> argProg <> singleton (Param rArg) --ACA
    where decons EmptyL = error "Empty sequence!"
          decons (l :< others) = (l,others)
          --getParam r True  = singleton (Param r) --Aquí se hace el access if b. ¿Capaz un ParamPointer?
          --getParam r False = singleton (Param r)


expToTac :: Exp -> TreeTranslator ((Program,Var))
expToTac (Unary op (ExpInt   a) ) = return  (empty , operateui op a )
expToTac (Unary op (ExpFloat a) ) = return  (empty , operateuf op a )
expToTac (ExpInt   i1) = return (empty,Int_Cons   i1) -- Single constant values
expToTac (ExpFloat i1) = return (empty,Float_Cons i1)
expToTac (ExpTrue    ) = return (empty,Int_Cons    1) -- gotoExpTrue
expToTac (ExpFalse   ) = return (empty,Int_Cons    0) -- gotoExpFalse
-- expToTac (Binary op (ExpInt i2) (ExpVar ev s)) = expToTac (Binary op (ExpVar ev s) (ExpInt i2))
  

-- Field access in structs and unions
expToTac node@(Binary Access exp1 exp2) = do
    itis <- isLval
    if itis 
    then do 
        nt <- newTemp
        let newIns = maybe (ReadArray (Temp nt) Fp (Int_Cons ofs) ) 
                           (\ s -> ReadArray (Temp nt) (MemAdress s) (Int_Cons ofs)) 
                           addr
        return (empty |> newIns , Temp nt)
    else do 
        (tac1,var1) <- expToTac exp1
        setRval
        (tac2,var2) <- expToTac exp2
        nt <- newTemp
        setLval -- No lo estamos recuperando
        return ((tac1 <> tac2) |> (Addi (Temp nt) var1 var2),(Temp nt))
  where (addr,ofs) = getStructOrUnion node 

    


-- Two integer constants or booleans
expToTac (Binary op (ExpInt i1) (ExpInt i2)) = do 
    let newVar = operatei op i1 i2
    -- must check if its smaller than 16-bits threshold signed. Maybe leave this for mips
    if ((-32768) <= (getCons newVar)) && ( (getCons newVar) <= 32767)
        then return (empty, newVar)
        else do nt <- newTemp
                return (empty |> (Mv (Temp nt) newVar) , (Temp nt))

expToTac (Unary Access ex) = do
    nt <- (newTemp >>= return . Temp)
    (ins,t) <- expToTac ex
    return ( ins |> (ReadPointer nt t) , nt)

expToTac (Unary I.Not ex) = do
    jAreSet <- jumpsAreSet
    
    if jAreSet 
        then do (lt,lf)  <- getJumps
                setTheseJumps lf lt
                return ( empty , Fp)
        else do (lt,lf)   <- setJumps
                nt        <- newTemp
                nl        <- newLabel
                return (jumpTrueFalse lf lt nl nt True, (Temp nt))


-- Two float constants
expToTac (Binary op (ExpFloat i1) (ExpFloat i2)) = do -- Two integer constants
    let newVar = operatef op i1 i2
    -- must check if its smaller than 16-bits threshold signed
    nt <- newTemp
    return (empty |> (Mv (Temp nt) newVar) , (Temp nt))

-- Single variable
expToTac (ExpVar dec s) = case (dir dec) of
        Label  ->        return( commentedIns , (MemAdress s  ) )
        (ThisLab str) -> return( commentedIns , (MemAdress str) )
        (Offset o) -> do 
            tempLocal <- newTemp
            let tl = Temp tempLocal
            itis <- isLval
            if itis
            then return ( commentedIns |> (Addi tl Fp (Int_Cons o)) , tl ) -- PELIGROSO
            else return ( commentedIns |> (ReadArray tl Fp (Int_Cons o)) , tl )
    where commentedIns = if debugVar then empty |> (Comment ("Variable " ++ s)) else empty
-- Generic unkwon operation
expToTac (Binary op exp1 exp2)
    | isCompare op = makeCompare exp1 exp2 op
    | isAO op      = makeBool    op exp1 exp2 
    | otherwise    = do 
        (ins1,t1) <- expToTac exp1
        (ins2,t2) <- expToTac exp2
        nt <- newTemp
        return ((ins1 <> ins2) |> (insTranslation op (Temp nt) t1 t2) ,Temp nt)
expToTac (CallVal s args size b ) = do
    tempLocal  <- newTemp
    prog <- argsToProg args b empty 
    return ((prog <> singleton (CallExp (Temp tempLocal) s size)),(Temp tempLocal))    

expToTac (Unary op a) = do 
    tempLocal  <- newTemp
    (ins,wher) <- expToTac a
    return (ins |> (insTranslation' op wher (Temp tempLocal)),(Temp tempLocal))

expToTac _ = return (singleton Nop,Temp 0)

makeCompare :: Exp -> Exp -> Operator -> TreeTranslator ((Program,Var))
makeCompare exp1 exp2 oper = do 
    jAreSet <- jumpsAreSet

    (epilogue,where_var)<- if jAreSet 
        then return (empty,Fp)
        else do (lt,lf) <- setJumps
                nl      <- newLabel
                nt      <- newTemp
                return ((jumpTrueFalse lt lf nl nt True),(Temp nt))

    (p1,v1) <- expToTac exp1
    (p2,v2) <- expToTac exp2
    (lt,lf) <- getJumps
    return ( (((p1 <> p2) |> (mkOp oper v1 v2 lt)) |> (Jump lf)) <> epilogue  , where_var )

makeBool :: Operator -> Exp -> Exp -> TreeTranslator ((Program,Var))
makeBool op exp1 exp2 = do 
    jAreSet <- jumpsAreSet
    
    if jAreSet 
        then do (lt,lf)   <- getJumps
                middleTag <- newLabel

                case op of 
                    I.And -> setTheseJumps middleTag lf
                    I.Or  -> setTheseJumps lt middleTag

                (p1,v1)    <- expToTac exp1
                setTheseJumps lt lf
                (p2,v2) <- expToTac exp2
                return ( (p1 |> (Tag middleTag)) <>  p2 , Fp)

        else do (lt,lf)   <- setJumps
                middleTag <- newLabel
                nl        <- newLabel
                nt        <- newTemp

                -- liftIO $ putStrLn "seteando ando"
                -- liftIO $ putStrLn $ show (lt,lf)


                case op of 
                    I.And -> setTheseJumps middleTag lf
                    I.Or  -> setTheseJumps lt middleTag

                (p1,v1)    <- expToTac exp1
                setTheseJumps lt lf
                (p2,v2) <- expToTac exp2

                let last_jump = case op of I.And -> False
                                           I.Or  -> True

                return ((p1 |> (Tag middleTag)) <> p2 <> (jumpTrueFalse lt lf nl nt last_jump) ,Temp nt)

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
-- insTranslation Array      = ReadArray
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

