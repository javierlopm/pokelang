module InsToTac(
    forestToTac,
    initTranslator,
    execTree,
    TranlatorState(..),
    TreeTranslator
) where

import Data.Sequence(empty,Seq,(|>),(<|))
import Data.Monoid((<>),mempty)
import Control.Applicative(pure)
import Data.Word(Word)
import Instructions
import Control.Monad.State
import Tac

data TranlatorState  = TranlatorState { tempCount  :: Word
                                      , labelCount :: Word }    
                                      --deriving(Show)      
type TreeTranslator  = StateT TranlatorState IO 

initTranslator :: TranlatorState
initTranslator = TranlatorState 0 0

-- use foldM instead
forestToTac :: [(String,Ins)] -> TreeTranslator ( [(String,Program)] )
forestToTac [] = return mempty
forestToTac ((str,insTree):tl)  = do 
    liftIO $ putStrLn $ show insTree
    headTac       <- treeToTac insTree
    forestTacTail <- forestToTac tl
    -- Maybe there'es no need to return, only write to file?
    return ( pure (str,headTac) <> forestTacTail)


treeToTac :: Ins -> TreeTranslator (Program)
treeToTac (Assign e1 e2) = do 
    (tac1,var1) <- expToTac e1
    (tac2,var2) <- expToTac e2
    let finaltac = (tac1 <> tac2) <> (pure (Mv var1 var2))
    liftIO $ putStrLn $ show finaltac
    return finaltac
treeToTac _ = return (pure Nop)

expToTac :: Exp -> TreeTranslator ((Program,Var))
expToTac _ = return (pure Nop,Temp 4)

-- Alias
execTree :: Monad m => StateT s m a -> s -> m s
execTree = execStateT