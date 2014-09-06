-- | Plot a curve of the throughputs (Mb/s) of packets transferred between n1 and n2.
--
-- > runhaskell Q4.hs < Q3.tr | gnuplot --persist
import Control.Applicative
import Control.Monad
import Text.Printf

import Data.Word
import TraceParser

main = do
    traces <- parse <$> getContents
    let packets = [ t | t <- traces, tType t == Received
                      , tDstLink t == 2 ]
        groupedSize = group20ms packets 0
    putStrLn "set xlabel 'Time (s)'"
    putStrLn "set ylabel 'Rate (Mb/s)'"
    putStrLn "plot '-' with lines title 'n2 received' lt rgb 'red'"

    forM_ groupedSize $ \(ms, sumSize) -> do
        printf "%.2f %f\n" ms (toMbps sumSize)
    putStrLn "e"
  where
    -- Sums trace packet sizes (by groups of 20 ms).
    group20ms :: [Trace] -> Double -> [(Double, Word)]
    group20ms packets currentMs =
        let currentMs' = currentMs + 0.020
            (this20ms, next) = span ((< currentMs') . tTime) packets
            sumSize = sum [ tSize t | t <- this20ms ]
            val = (currentMs, sumSize)
        in if null next then [ val ]
                        else val : group20ms next currentMs'

    double :: Integral a => a -> Double
    double = fromIntegral

    toMbps bytes = double (bytes * 50) * 8 / 1024^2
