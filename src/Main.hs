{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Applicative (empty)
import Control.Arrow ((***))
import Data.Bifunctor (second)
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set

main = do
    putStrLn "Advent Of Code"
    day1
    day2

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

{-
--- Day 2: Password Philosophy ---
Your flight departs in a few days from the coastal airport; the easiest way
down to the coast from here is via toboggan.

The shopkeeper at the North Pole Toboggan Rental Shop is having a bad day.
"Something's wrong with our computers; we can't log in!" You ask if you can
take a look.

Their password database seems to be a little corrupted: some of the passwords
wouldn't have been allowed by the Official Toboggan Corporate Policy that was
in effect when they were chosen.

To try to debug the problem, they have created a list (your puzzle input) of
passwords (according to the corrupted database) and the corporate policy when
that password was set.

For example, suppose you have the following list:

1-3 a: abcde 1-3 b: cdefg 2-9 c: ccccccccc Each line gives the password policy
and then the password. The password policy indicates the lowest and highest
number of times a given letter must appear for the password to be valid. For
example, 1-3 a means that the password must contain a at least 1 time and at
most 3 times.

In the above example, 2 passwords are valid. The middle password, cdefg, is
not; it contains no instances of b, but needs at least 1. The first and third
passwords are valid: they contain one a or nine c, both within the limits of
their respective policies.

How many passwords are valid according to their policies?

--- Part Two ---

While it appears you validated the passwords correctly, they don't
seem to be what the Official Toboggan Corporate Authentication System
is expecting.

The shopkeeper suddenly realizes that he just accidentally explained
the password policy rules from his old job at the sled rental place
down the street! The Official Toboggan Corporate Policy actually works
a little differently.

Each policy actually describes two positions in the password, where 1
means the first character, 2 means the second character, and so on.
(Be careful; Toboggan Corporate Policies have no concept of "index
zero"!) Exactly one of these positions must contain the given letter.
    Other occurrences of the letter are irrelevant for the purposes of
    policy enforcement.

Given the same example list from above:

1-3 a: abcde is valid: position 1 contains a and position 3 does not.
    1-3 b: cdefg is invalid: neither position 1 nor position 3
    contains b.  2-9 c: ccccccccc is invalid: both position 2 and
    position 9 contain c.  How many passwords are valid according to
    the new interpretation of the policies?
-}

data Rule = Rule
    { rule_number1 :: Int
    , rule_number2 :: Int
    , rule_letter :: Char
    }

type Password = String

day2 :: IO ()
day2 = do
    passwords <- fmap readPassword . lines <$> readFile "puzzle-inputs/2.txt"

    putStrLn "Day 2 - 1"
    putStrLn $
        "Passwords that satisfy the rules for min and max: "
            <> show (length $ filter satisfiesRuleMinMax passwords)

    putStrLn "Day 2 - 1"
    putStrLn $
        "Passwords that satisfy the rules for index positions: "
            <> show (length $ filter satisfiesRulePositions passwords)
    where
        readPassword :: String -> (Rule , String)
        readPassword (words -> [numbers , [letter , ':'] , password]) =
            (Rule number1 number2 letter , password)
            where
                (number1 , number2) = (read *** read) $ second tail $ break (== '-') numbers

        satisfiesRuleMinMax :: (Rule , String) -> Bool
        satisfiesRuleMinMax (Rule minCount maxCount letter , password) =
            minCount <= letterCount && letterCount <= maxCount
            where
                letterCount = length $ filter (== letter) password

        satisfiesRulePositions :: (Rule , String) -> Bool
        satisfiesRulePositions (Rule ix1 ix2 letter , password) =
            (1 ==) $
                length
                    [ pletter
                    | (ix , pletter) <- zip [1 ..] password
                    , ix == ix1 || ix == ix2
                    , pletter == letter
                    ]
