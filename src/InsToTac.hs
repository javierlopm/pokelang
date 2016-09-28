module InsToTac(
    ) where

import Data.Sequence(empty,Seq,(|>),(<|))
import Instructions
import Control.Monad.State.Strict
import Tac

type treeTranslator      = MonadState TranlatorState
-- Sospecho que habrá que hacer un liftIO místico
data TranlatorState  = TranlatorState { tempCount  :: Word
                                      , labelCount :: Word
                                      , insForest  :: [(String,Ins)],
                                      , tacList    :: [(String,IntIns)]}

-- Transform each instruction tree to Three Address Code
--treeToTac :: [(String,Ins)] -> (String, Program)
--treeToTac  = (buildList) . (foldl processIns (Nothing,[]) )
--    where processIns (a,l) (string,insTree) = if string == "hitMAINlee" 
          

forestToTac :: treeTranslator (Int)
forestToTac = undefined