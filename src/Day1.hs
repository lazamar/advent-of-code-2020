module Day1 where

import Data.List as List
import Data.Maybe as M
import Data.Set as Set

main :: IO ()
main = do
    numbers <- fmap read . lines <$> readFile "puzzle-inputs/1.txt"
    putStrLn $
        unlines
            [ "Day 1"
            , case twoNumbersThatAddTo 2020 numbers of
                  Nothing -> "No two numbers add to 2020"
                  Just (one , two) ->
                      unwords [show one , "and" , show two , "which multiply to" , show (one * two)]
            , case threeNumbersThatAddTo 2020 numbers of
                  Nothing -> "No three numbers add to 2020"
                  Just (one , two , three) ->
                      unwords [show one , "," , show two , ", and" , show three , "which multiply to" , show (one * two * three)]
            ]

-- O(n)
twoNumbersThatAddTo :: Int -> [Int] -> Maybe (Int , Int)
twoNumbersThatAddTo target numbers = M.listToMaybe $ do
    let nset = Set.fromList numbers
    n <- numbers
    let complement = target - n
    if complement `Set.member` nset
        then return (n , complement)
        else mempty

-- O(n^2)
threeNumbersThatAddTo :: Int -> [Int] -> Maybe (Int , Int , Int)
threeNumbersThatAddTo target numbers = listToMaybe $ do
    x <- numbers
    Just (y , z) <- pure $ twoNumbersThatAddTo (target - x) $ List.filter (/= x) numbers
    return (x , y , z)
