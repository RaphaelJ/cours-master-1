-- | Determine the average throughput of TCP on the link between n0 and n1 for
-- the last 3 seconds of the simulation.
--
-- > runhaskell Q1_2.hs < Q1_1.tr
import Control.Applicative
import Text.Printf

import TraceParser

main = do
    traces <- parse <$> getContents
    let transfered = sum [ tSize t | t <- dropWhile ((< 2.0) . tTime) traces
                                   , tSrcLink t == 0, tDstLink t == 1
                                   , tType t == Received, tName t == "tcp" ]
    printf "Bits par second : %d\n" (transfered `quot` 3 * 8)
