module TraceParser (Trace (..), TraceType (..), parse, parseLine) where

import Data.Maybe
import Data.Word

data Trace = Trace {
      tType    :: TraceType
    , tTime    :: Double
    , tSrcLink :: Int
    , tDstLink :: Int
    , tName    :: String
    , tSize    :: Word
    , tFlag    :: String
    , tClass   :: Int
    , tSrcNode :: (Int, Int) -- | (Source node, source port)
    , tDstNode :: (Int, Int) -- | (Dest node, dest port)
    , tSeq     :: Word       -- | TCP Sequence number.
    , tUnique  :: Word       -- | Simulator unique packet number.
    } deriving (Show)

data TraceType = Received | Lost | EnterQueue | LeaveQueue deriving (Show, Eq)

parse :: String -> [Trace]
parse = catMaybes . map parseLine . lines

parseLine :: String -> Maybe Trace
parseLine line
    | length ws /= 12 = Nothing
    | otherwise       = Just $
        Trace typ' (read time) (read srcLink) (read dstLink) name (read size)
                   flags (read clas) (parseNode srcNode) (parseNode dstNode)
                   (read sequ) (read unique)
  where
    ws = words line

    [ typ, time, srcLink, dstLink, name, size, flags, clas, srcNode
         , dstNode, sequ, unique ] = ws

    typ' = case typ of "r" -> Received
                       "d" -> Lost
                       "+" -> EnterQueue
                       "-" -> LeaveQueue

    parseNode node = let (ip, (_:port)) = break (== '.') node
                     in (read ip, read  port)
