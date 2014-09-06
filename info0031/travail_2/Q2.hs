-- | Plot a curve of the throughputs (Mb/s) of the three TCP flows at the source
-- node.
--
-- Without min-max fairness :
-- > runhaskell Q2.hs < Q2.tr | gnuplot --persist
-- With min-max fairness :
-- > runhaskell Q2.hs < Q6.tr | gnuplot --persist
import Control.Applicative
import Control.Monad
import Text.Printf

import Data.Word
import TraceParser

main = do
    traces <- parse <$> getContents
    let tcpPackets = [ t | t <- traces, tType t == LeaveQueue
                         , tName t == "tcp" ]
        groupedSize = groupCenti tcpPackets 0

    putStrLn "set xlabel 'Time (seconds)'"
    putStrLn "set ylabel 'Throughput (Mbps)'"

    putStrLn "plot '-' with lines title 'TCP0' lt rgb 'red', \
                  \'-' with lines title 'TCP1' lt rgb 'blue', \
                  \'-' with lines title 'TCP2' lt rgb 'green'"

    forM_ groupedSize $ \(centi, sumTcp0, _, _) -> do
        printf "%.2f %f\n" centi (toMbps sumTcp0)
    putStrLn "e"

    forM_ groupedSize $ \(centi, _, sumTcp1, _) -> do
        printf "%.2f %f\n" centi (toMbps sumTcp1)
    putStrLn "e"

    forM_ groupedSize $ \(centi, _, _, sumTcp2) -> do
        printf "%.2f %f\n" centi (toMbps sumTcp2)
    putStrLn "e"

  where
    -- Sums trace packet sizes (by groups of a deca-second).
    groupCenti :: [Trace] -> Double -> [(Double, Word, Word, Word)]
    groupCenti packets centi =
        let centi' = centi + 0.1
            (thisCenti, next) = span ((< centi') . tTime) packets
            sumTcp0 = sum [ tSize t | t <- thisCenti, tSrcLink t == 13 ]
            sumTcp1 = sum [ tSize t | t <- thisCenti, tSrcLink t == 11 ]
            sumTcp2 = sum [ tSize t | t <- thisCenti, tSrcLink t == 9 ]
            val = (centi, sumTcp0, sumTcp1, sumTcp2)
        in if null next then [ val ]
                        else val : groupCenti next centi'

    double :: Integral a => a -> Double
    double = fromIntegral

    toMbps bits = double (bits * 10) / 1024^2 * 8
