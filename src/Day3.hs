{-# LANGUAGE LambdaCase #-}

module Day3 where

import Data.Array as Array
import Data.Tuple (swap)

type Matrix a = Array (Int , Int) a

type Point = (Int , Int)

type Slope = (Int , Int)

data GridItem
    = Square
    | Tree
    deriving (Eq , Show)

main :: IO ()
main = do
    grid <- parseFile <$> readFile "puzzle-inputs/3.txt"
    let slopes = [(1 , 1) , (3 , 1) , (5 , 1) , (7 , 1) , (1 , 2)]
    putStrLn $
        unlines
            [ "Day 3"
            , "Total trees in slope (3, 1): " <> show (treeCount grid (3 , 1))
            , "Multiplication of tree slopes: " <> show (product $ map (treeCount grid) slopes)
            ]

parseFile :: String -> Matrix GridItem
parseFile = fmap toGridItem . readMatrix . lines

toGridItem :: Char -> GridItem
toGridItem = \case
    '.' -> Square
    '#' -> Tree

readMatrix :: [[a]] -> Matrix a
readMatrix items = Array.ixmap range swap $ Array.listArray (swap <$> range) $ concat items
    where
        range = ((0 , 0) , (maxX , maxY))
        maxY = length items - 1
        maxX = length (head items) - 1

treeCount :: Matrix GridItem -> Slope -> Int
treeCount grid slope =
    length
        . filter (== Tree)
        . map (grid Array.!)
        $ path grid slope (0 , 0)

-- O(y) where y is the height of the matrix.
-- Without using an Array the worst case time complexity would
-- be O(x*y) for each path retrieval.
path :: Matrix a -> Slope -> Point -> [Point]
path matrix slope origin = unroll (move slope) origin
    where
        move :: Slope -> Point -> Maybe Point
        move (right , down) (x , y)
            | y + down > maxY = Nothing
            | otherwise = Just ((x + right) `mod` (maxX + 1) , y + down)

        (_ , (maxX , maxY)) = Array.bounds matrix

unroll :: (a -> Maybe a) -> a -> [a]
unroll f x = x : maybe [] (unroll f) (f x)
