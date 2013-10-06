-- | Plot a curve of the throughput (Mb/s) of the data sent by n0 as a function
-- of time (s) for the last 3 seconds of the simulation.
--
-- > runhaskell Q1_6.hs < Q1_3.tr | gnuplot --persist
import Control.Applicative
import Control.Monad
import Text.Printf

import Data.Word
import TraceParser

main = do
    traces <- parse <$> getContents
    let packets = [ t | t <- traces, tSrcLink t == 0, tTime t >= 2.0 ]
        groupedSize = groupCenti packets 2.0

    putStrLn "plot '-' with lines title 'TCP bandwidth (Mbps)' lt rgb 'green', \
                  \'-' with lines title 'UDP bandwidth (Mbps)' lt rgb 'blue', \
                  \'-' with lines title 'Total bandwidth (Mbps)' lt rgb 'red'"

    forM_ groupedSize $ \(centi, sumTcp, _, _) -> do
        putStrLn "# centisecond tcp"
        printf "%.2f %f\n" centi (toMbps sumTcp)
    putStrLn "e"

    forM_ groupedSize $ \(centi, _, sumUdp, _) -> do
        putStrLn "# centisecond udp"
        printf "%.2f %f\n" centi (toMbps sumUdp)
    putStrLn "e"

    forM_ groupedSize $ \(centi, _, _, sumTotal) -> do
        putStrLn "# centisecond total"
        printf "%.2f %f\n" centi (toMbps sumTotal)
    putStrLn "e"
  where
    -- Sums trace packet sizes (by groups of a centi-second).
    groupCenti :: [Trace] -> Double -> [(Double, Word, Word, Word)]
    groupCenti packets centi =
        let centi' = centi + 0.01
            (thisCenti, next) = span ((< centi') . tTime) packets
            sumTcp = sum [ tSize t | t <- thisCenti, tName t == "tcp" ]
            sumUdp = sum [ tSize t | t <- thisCenti, tName t == "cbr" ]
            sumTotal = sum $ map tSize thisCenti
            val = (centi, sumTcp, sumUdp, sumTotal)
        in if null next then [ val ]
                        else val : groupCenti next centi'

    double :: Integral a => a -> Double
    double = fromIntegral

    toMbps bits = double (bits * 100) / 1024^2
