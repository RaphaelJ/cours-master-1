-- |  For both routing modes, give the average throughput of TCP (between
-- t = 0.5 and t = 4.5 s).
--
-- For the sparse mode:
-- > runhaskell Q2_4.hs < Q2_4_sparse.tr
-- For the dense mode:
-- > runhaskell Q2_4.hs < Q2_4_dense.tr
import Control.Applicative
import Text.Printf

import TraceParser

main = do
    traces <- parse <$> getContents
    let transfered = sum [ tSize t | t <- takeWhile ((<= 4.5) . tTime)
                                        $ dropWhile ((< 0.5)  . tTime) traces
                                   , tSrcLink t == 7, tDstLink t == 5
                                   , tType t == Received, tName t == "tcp" ]

    printf "Bits par second : %d\n" (transfered `quot` 4 * 8)
