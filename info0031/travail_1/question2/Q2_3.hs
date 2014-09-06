-- | Determine the average throughput of TCP on the link between n0 and n1 for
-- the last 3 seconds of the simulation.
--
-- For the sparse mode:
-- > runhaskell Q2_3.hs < Q2_1_sparse.tr
-- For the dense mode:
-- > runhaskell Q2_3.hs < Q2_1_dense.tr
import Control.Applicative
import Text.Printf

import TraceParser

main = do
    traces <- parse <$> getContents
    let received = [ t | t <- traces, tType t == Received ]
        grafts = length [ t | t <- received, tName t == "graft" ]
        prunes = length [ t | t <- received, tName t == "prune" ]
    printf "Grafts : %d - Prunes : %d\n" grafts prunes
