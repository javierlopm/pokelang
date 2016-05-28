module ErrorHandle
    where 

import Tokens(Token(..))
import System.IO(hPutStrLn,stderr)
import qualified Data.Foldable as F(foldr)
import Data.Sequence(Seq)

-- Check token list for TkError and count them
checkTokenError :: [Token] -> ([Token],[Token],Int)
checkTokenError = foldr pickGoods ([],[],0)
    where pickGoods myTok@TkError{} (gs,bs,bcount) = (gs,myTok:bs,bcount+1)
          pickGoods myTok           (gs,bs,bcount) = (myTok:gs,bs,bcount)

-- Divide parser log in logs, errors and error count
checkParseError :: Seq (Either String String) -> ([String],[String],Int)
checkParseError = F.foldr pick ([],[],0)
    where pick (Left str)  (gs,bds,count) = (gs,str:bds,count+1)
          pick (Right str) (gs,bds,count) = (str:gs,bds,count)

-- Print error count and errors to stderr
printErrors :: Show a => Int -> (a->String) -> [a] -> IO()
printErrors c f l = do 
    mapM_ ((hPutStrLn  stderr) . f) l
    hPutStrLn  stderr $ "-pkcc:"++ show c ++ if c > 1 then " errors found."
                                                      else " error found"