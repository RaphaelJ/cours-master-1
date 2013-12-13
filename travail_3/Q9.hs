-- | Compute the maximum delay experienced by the packets of the video flow.
--
-- > runhaskell Q9.hs < Q8.tr
import Control.Applicative
import Data.Function
import Data.List
import Text.Printf

import TraceParser

main = do
    traces <- parse <$> getContents
    let packets = [ t | t <- traces, tName t == "exp"
                      , (tSrcLink t == 1 && tType t == EnterQueue)
                        || (tDstLink t == 10 && tType t == Received)
                        || (tType t == Lost) ]

    printf "Maximum delay: %f sec.\n" (maximum (delays packets))
  where
    delays []                       = []
    delays (p:ps) | tSrcLink p /= 1 = delays ps
                  | otherwise       =
        case find ((== tSeq p) . tSeq) ps of
            Just p' |    tType p' == Received
		      && tDstLink p' == 10    -> (tTime p' - tTime p) : delays ps
            _                                 -> delays ps
