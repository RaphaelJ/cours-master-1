-- | Determine the number of packets each flow has lost during the whole
-- simulation and the average rates of each flow
--
-- > runhaskell Q1.hs < Q1.tr
import Control.Applicative
import Text.Printf

import TraceParser

main = do
    traces <- parse <$> getContents
    let dropped = filter ((== Lost)  . tType) traces
        droppedTCP1 = length $ filter ((== 2) . tClass) dropped
	droppedTCP2 = length $ filter ((== 3) . tClass) dropped
        droppedudp = length $ filter ((== 0) . tClass) dropped
	droppedvideo = length $ filter ((== 1) . tClass) dropped

    printf "TCP1 packets dropped: %d\n" droppedTCP1
    printf "TCP2 packets dropped: %d\n" droppedTCP2
    printf "udp packets dropped: %d\n" droppedudp 
    printf "video packets dropped: %d\n" droppedvideo

    let transfered_TCP1 = sum [ tSize t | t <- dropWhile ((< 2.0) . tTime) traces
                                   , tDstLink t == 10
                                   , tType t == Received, tName t == "tcp" ]
    printf "Bits per second for tcp1 : %d\n" (transfered_TCP1)

    let transfered_TCP2 = sum [ tSize t | t <- dropWhile ((< 2.0) . tTime) traces
                                   , tDstLink t == 9
                                   , tType t == Received, tName t == "tcp" ]
    printf "Bits per second for tcp2: %d\n" (transfered_TCP2)

    let transfered_udp = sum [ tSize t | t <- dropWhile ((< 2.0) . tTime) traces
                                   , tDstLink t == 7
                                   , tType t == Received, tName t == "cbr" ]
    printf "Bits per second for udp: %d\n" (transfered_udp)

    let transfered_video = sum [ tSize t | t <- dropWhile ((< 2.0) . tTime) traces
                                   , tDstLink t == 10
                                   , tType t == Received, tName t == "exp" ]
    printf "Bits per second for video: %d\n" (transfered_video)


