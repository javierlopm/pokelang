module InsToTac(
    forestToTac,
    TranlatorState
    ) where

--ghci -i InsToTac.hs Tac.hs Instructions.hs ../types/Types.hs ../tokens/Tokens.hs ../symtable/TableTree.hs

import Data.Sequence(empty,Seq,(|>),(<|))
import Data.Monoid((<>),mempty)
import Control.Applicative(pure)
import Data.Word(Word)
import Instructions
import Control.Monad.State
import Tac

type TreeTranslator  = StateT TranlatorState IO 
data TranlatorState  = TranlatorState { tempCount  :: Word
                                      , labelCount :: Word }    
                                      deriving(Show)      

-- use foldM instead
forestToTac :: [(String,Ins)] -> TreeTranslator ([(String,IntIns)])
forestToTac [] = return mempty
forestToTac ((str,insTree):tl)  = do 
    headTac       <- treeToTac insTree
    forestTacTail <- forestToTac tl
    -- Maybe there'es no need to return, only write to file?
    return ( pure (str,headTac) <> forestTacTail)


treeToTac :: Ins -> TreeTranslator (IntIns)
treeToTac _ = return Nop

treeToTac' :: Exp -> TreeTranslator((IntIns,Var))
treeToTac' _ = return (Nop,Temp 4)

--insToTac :: Ins ->  -> IntIns