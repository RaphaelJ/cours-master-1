-- | Determine the number of packets each flow has lost during the whole
-- simulation.
--
-- > runhaskell Q1_5.hs < Q1_3.tr
import Control.Applicative
import Text.Printf

import TraceParser

main = do
    traces <- parse <$> getContents
    let dropped = filter ((== Lost)  . tType) traces
        droppedTCP = length $ filter ((== 2) . tClass) dropped
        droppedUDP = length $ filter ((== 0) . tClass) dropped

    printf "Last packet dropped at %f\n" (tTime $ last dropped)
    printf "TCP packets dropped: %d\n" droppedTCP
    printf "UDP packets dropped: %d\n" droppedUDP
