{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Applicative (empty)
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set

main = do
    putStrLn "Advent Of Code"
    day1

{-- Day 1
After saving Christmas five years in a row, you've decided to take a vacation
at a nice resort on a tropical island. Surely, Christmas will go on without
you.

The tropical island has its own currency and is entirely cash-only. The gold
coins used there have a little picture of a starfish; the locals just call them
stars. None of the currency exchanges seem to have heard of them, but somehow,
you'll need to find fifty of these coins by the time you arrive so you can pay
the deposit on your room.

To save your vacation, you need to get all fifty stars by December 25th.

Collect stars by solving puzzles. Two puzzles will be made available on each
day in the Advent calendar; the second puzzle is unlocked when you complete the
first. Each puzzle grants one star. Good luck!

Before you leave, the Elves in accounting just need you to fix your expense
report (your puzzle input); apparently, something isn't quite adding up.

Specifically, they need you to find the two entries that sum to 2020 and then
multiply those two numbers together.

For example, suppose your expense report contained the following:

1721 979 366 299 675 1456 In this list, the two entries that sum to 2020 are
1721 and 299. Multiplying them together produces 1721 * 299 = 514579, so the
correct answer is 514579.

Of course, your expense report is much larger. Find the two entries that sum to
2020; what do you get if you multiply them together?
-}

day1 :: IO ()
day1 = do
    numbers <- fmap read . lines <$> readFile "puzzle-inputs/1.txt"

    putStrLn "Day 1 - 1"
    putStrLn $ case twoNumbersThatAddTo 2020 numbers of
        Nothing -> "No two numbers add to 2020"
        Just (one , two) ->
            unwords
                [ "The answer is "
                , show one
                , "and"
                , show two
                , "which multiply to"
                , show (one * two)
                ]

    putStrLn "Day 1 - 2"
    putStrLn $ case threeNumbersThatAddTo 2020 numbers of
        Nothing -> "No three numbers add to 2020"
        Just (one , two , three) ->
            unwords
                [ "The answer is "
                , show one
                , ","
                , show two
                , ", and"
                , show three
                , "which multiply to"
                , show (one * two * three)
                ]
    where
        -- O(n)
        twoNumbersThatAddTo :: (Ord a , Num a) => a -> [a] -> Maybe (a , a)
        twoNumbersThatAddTo target (Set.fromList -> numbers) = listToMaybe $ do
            n <- Set.toList numbers
            let complement = target - n
            if complement `Set.member` numbers
                then return (n , complement)
                else empty

        -- O(n^2)
        threeNumbersThatAddTo :: (Ord a , Num a) => a -> [a] -> Maybe (a , a , a)
        threeNumbersThatAddTo target numbers = listToMaybe $ do
            n1 <- numbers
            Just (n2 , n3) <- pure $ twoNumbersThatAddTo (target - n1) $ filter (/= n1) numbers
            return (n1 , n2 , n3)
