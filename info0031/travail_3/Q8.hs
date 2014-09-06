-- | Determine the average rates of each flow on the link n3n6 and n6n8
--
-- > runhaskell Q9.hs < Q8.tr
import Control.Applicative
import Text.Printf

import TraceParser

main = do
    traces <- parse <$> getContents

    let transfered_TCP1_n6n8 = sum [ tSize t | t <- dropWhile ((< 2.0) . tTime) traces
                                   , tSrcLink t == 6, tDstLink t == 8, tClass t == 2
                                   , tType t == Received, tName t == "tcp" ]
    printf "Bits per second for tcp1 on the link n6 n8: %d\n" (transfered_TCP1_n6n8)

    let transfered_TCP2_n3n6 = sum [ tSize t | t <- dropWhile ((< 2.0) . tTime) traces
                                   , tSrcLink t == 3, tDstLink t == 6, tClass t == 3
                                   , tType t == Received, tName t == "tcp" ]
    printf "Bits per second for tcp2 on the link n3 n6: %d\n" (transfered_TCP2_n3n6)

    let transfered_TCP2_n6n8 = sum [ tSize t | t <- dropWhile ((< 2.0) . tTime) traces
				   , tSrcLink t == 6, tDstLink t == 8, tClass t == 3
                                   , tType t == Received, tName t == "tcp" ]
    printf "Bits per second for tcp2 on the link n6 n8: %d\n" (transfered_TCP2_n6n8)

    let transfered_TCP3_n3n6 = sum [ tSize t | t <- dropWhile ((< 2.0) . tTime) traces
                                   , tSrcLink t == 3, tDstLink t == 6, tClass t == 4
                                   , tType t == Received, tName t == "tcp" ]
    printf "Bits per second for tcp3 on the link n3 n6: %d\n" (transfered_TCP3_n3n6)

    let transfered_TCP3_n6n8 = sum [ tSize t | t <- dropWhile ((< 2.0) . tTime) traces
				   , tSrcLink t == 6, tDstLink t == 8, tClass t == 4
                                   , tType t == Received, tName t == "tcp" ]
    printf "Bits per second for tcp3 on the link n6 n8: %d\n" (transfered_TCP3_n6n8)

    let transfered_udp_n3n6 = sum [ tSize t | t <- dropWhile ((< 2.0) . tTime) traces
                                   , tSrcLink t == 3, tDstLink t == 6
                                   , tType t == Received, tName t == "cbr" ]
    printf "Bits per second for udp on the link n3 n6: %d\n" (transfered_udp_n3n6)

    let transfered_video_n3n6 = sum [ tSize t | t <- dropWhile ((< 2.0) . tTime) traces
                                   , tSrcLink t == 3, tDstLink t == 6
                                   , tType t == Received, tName t == "exp" ]
    printf "Bits per second for video on the link n3 n6: %d\n" (transfered_video_n3n6)

    let transfered_video_n6n8 = sum [ tSize t | t <- dropWhile ((< 2.0) . tTime) traces
                                   , tSrcLink t == 6, tDstLink t == 8
                                   , tType t == Received, tName t == "exp" ]
    printf "Bits per second for video on the link n6 n8: %d\n" (transfered_video_n6n8)


