{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Applicative (empty)
import Control.Arrow ((***))
import Data.Array (Array)
import Data.Bifunctor (second)
import Data.Maybe (catMaybes, isJust, listToMaybe)
import qualified Data.Array as Array
import qualified Data.Set as Set

main = do
    putStrLn "Advent Of Code"
    day1
    day2
    day3

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

data Rule = Rule Int Int Char

type Password = String

day2 :: IO ()
day2 = do
    passwords <- fmap readPassword . lines <$> readFile "puzzle-inputs/2.txt"
    putStrLn $
        unlines
            [ "Day 2"
            , "Passwords that satisfy the rules for min and max: "
                  <> show (length $ filter satisfiesRuleMinMax passwords)
            , "Passwords that satisfy the rules for index positions: "
                  <> show (length $ filter satisfiesRulePositions passwords)
            ]
    where
        readPassword :: String -> (Rule , Password)
        readPassword (words -> [numbers , [letter , ':'] , password]) =
            (Rule number1 number2 letter , password)
            where
                (number1 , number2) = (read *** read) $ second tail $ break (== '-') numbers

        satisfiesRuleMinMax :: (Rule , Password) -> Bool
        satisfiesRuleMinMax (Rule minCount maxCount letter , password) =
            minCount <= letterCount && letterCount <= maxCount
            where
                letterCount = length $ filter (== letter) password

        satisfiesRulePositions :: (Rule , Password) -> Bool
        satisfiesRulePositions (Rule ix1 ix2 letter , password) =
            length charsAtIndices == 1
            where
                charsAtIndices =
                    [ char
                    | (ix , char) <- zip [1 ..] password
                    , ix == ix1 || ix == ix2
                    , char == letter
                    ]

{-
--- Day 3: Toboggan Trajectory ---

With the toboggan login problems resolved, you set off toward the airport.
While travel by toboggan might be easy, it's certainly not safe: there's very
minimal steering and the area is covered in trees. You'll need to see which
angles will take you near the fewest trees.

Due to the local geology, trees in this area only grow on exact integer
coordinates in a grid. You make a map (your puzzle input) of the open squares
(.) and trees (#) you can see. For example:

..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#

These aren't the only trees, though; due to something you read about once
involving arboreal genetics and biome stability, the same pattern repeats to
the right many times:

..##.........##.........##.........##.........##.........##.......  --->
#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
.#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
.#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....  --->
.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
.#........#.#........#.#........#.#........#.#........#.#........#
#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...
#...##....##...##....##...##....##...##....##...##....##...##....#
.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#  --->

You start on the open square (.) in the top-left corner and need to reach the
bottom (below the bottom-most row on your map).

The toboggan can only follow a few specific slopes (you opted for a cheaper
model that prefers rational numbers); start by counting all the trees you would
encounter for the slope right 3, down 1:

From your starting position at the top-left, check the position that is right 3
and down 1. Then, check the position that is right 3 and down 1 from there, and
so on until you go past the bottom of the map.

The locations you'd check in the above example are marked here with O where
there was an open square and X where there was a tree:

..##.........##.........##.........##.........##.........##.......  --->
#..O#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
.#....X..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
..#.#...#O#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
.#...##..#..X...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
..#.##.......#.X#.......#.##.......#.##.......#.##.......#.##.....  --->
.#.#.#....#.#.#.#.O..#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
.#........#.#........X.#........#.#........#.#........#.#........#
#.##...#...#.##...#...#.X#...#...#.##...#...#.##...#...#.##...#...
#...##....##...##....##...#X....##...##....##...##....##...##....#
.#..#...#.#.#..#...#.#.#..#...X.#.#..#...#.#.#..#...#.#.#..#...#.#  --->

In this example, traversing the map using this slope would cause you to
encounter 7 trees.

Starting at the top-left corner of your map and following a slope of right 3
and down 1, how many trees would you encounter?
-}

type Matrix a = Array (Int , Int) a

type Point = (Int , Int)

type Slope = (Int , Int)

data GridItem
    = Square
    | Tree
    deriving (Eq , Show)

day3 :: IO ()
day3 = do
    grid <- parseFile <$> readFile "puzzle-inputs/3.txt"
    let slopes = [(1 , 1) , (3 , 1) , (5 , 1) , (7 , 1) , (1 , 2)]
    putStrLn $
        unlines
            [ "Day 3"
            , "Total trees in slope (3, 1): " <> show (treeCount grid (3 , 1))
            , "Multiplication of tree slopes: " <> show (product $ map (treeCount grid) slopes)
            ]
    where
        parseFile :: String -> Matrix GridItem
        parseFile = fmap toGridItem . readMatrix . lines

        toGridItem :: Char -> GridItem
        toGridItem = \case
            '.' -> Square
            '#' -> Tree

        readMatrix :: [[a]] -> Matrix a
        readMatrix ll =
            Array.array
                ((0 , 0) , (maxX , maxY))
                [ ((x , y) , val)
                | (y , row) <- zip [0 ..] ll
                , (x , val) <- zip [0 ..] row
                ]
            where
                maxY = length ll - 1
                maxX = (length $ head ll) - 1

        treeCount :: Matrix GridItem -> Slope -> Int
        treeCount grid slope =
            length
                . filter (== Tree)
                . map (grid Array.!)
                $ path grid slope (0 , 0)

        path :: Matrix a -> Slope -> Point -> [Point]
        path matrix slope =
            catMaybes
                . takeWhile isJust
                . iterate (>>= move slope)
                . Just
            where
                move :: Slope -> Point -> Maybe Point
                move (right , down) (x , y)
                    | y + down > maxY = Nothing
                    | otherwise = Just ((x + right) `mod` (maxX + 1) , y + down)

                (_ , (maxX , maxY)) = Array.bounds matrix
