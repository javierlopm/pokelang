module InsToTac(
    ) where

import Data.Sequence(empty,Seq,(|>),(<|))
import Data.Word(Word)
import Instructions
import Control.Monad.State
import Tac

-- Sospecho que habrá que hacer un liftIO místico
type TreeTranslator  = StateT TranlatorState IO 
data TranlatorState  = TranlatorState { tempCount  :: Word
                                      , labelCount :: Word
                                      , insForest  :: [(String,Ins)]
                                      , tacList    :: [(String,IntIns)]}    
                                      deriving(Show)      

forestToTac :: TreeTranslator ()
forestToTac = do liftIO $ print "hey"
                 return ()
