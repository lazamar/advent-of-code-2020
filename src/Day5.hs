module Day5 where

import Control.Arrow ((***))
import qualified Data.Set as Set

data Bit = One | Zero

main :: IO ()
main = do
    seatIds <- map (seatId . codeToRowColumn) . lines <$> readFile "puzzle-inputs/5.txt"
    putStrLn $
        unlines
            [ "Day 5"
            , "Highest seat id " <> show (maximum seatIds)
            , "My seat number is " <> show (mySeatNumber seatIds)
            ]

seatId :: (Int , Int) -> Int
seatId (row , col) = row * 8 + col

codeToRowColumn :: String -> (Int , Int)
codeToRowColumn =
    both binaryToDecimal
        . (map rowCharToBit *** map colCharToBit)
        . span (`elem` "FB")

both f (a , b) = (f a , f b)

rowCharToBit 'F' = Zero
rowCharToBit 'B' = One

colCharToBit 'L' = Zero
colCharToBit 'R' = One

binaryToDecimal :: [Bit] -> Int
binaryToDecimal = foldr step 0 . zip [0 ..] . reverse
    where
        step (pos , One) acc = acc + 2 ^ pos
        step (_ , Zero) acc = acc

mySeatNumber :: [Int] -> Int
mySeatNumber seatList = head $ filter hasNeighbours missingSeats
    where
        seats = Set.fromList seatList
        Just minId = Set.lookupMin seats
        Just maxId = Set.lookupMax seats
        missingSeats = filter (not . (`Set.member` seats)) [minId , minId + 1 .. maxId]
        hasNeighbours sid = Set.member (sid - 1) seats && Set.member (sid + 1) seats
