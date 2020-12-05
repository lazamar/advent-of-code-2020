{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Applicative (empty)
import Control.Arrow ((***))
import Data.Array (Array)
import Data.Bifunctor (second)
import Data.Char (isDigit)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Tuple (swap)
import qualified Data.Array as Array
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

main :: IO ()
main = do
    putStrLn "Advent Of Code"
    day1
    day2
    day3
    day4

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
        twoNumbersThatAddTo :: Int -> [Int] -> Maybe (Int , Int)
        twoNumbersThatAddTo target numbers = listToMaybe $ do
            let nset = Set.fromList numbers
            n <- numbers
            let complement = target - n
            if complement `Set.member` nset
                then return (n , complement)
                else empty

        -- O(n^2)
        threeNumbersThatAddTo :: Int -> [Int] -> Maybe (Int , Int , Int)
        threeNumbersThatAddTo target numbers = listToMaybe $ do
            x <- numbers
            Just (y , z) <- pure $ twoNumbersThatAddTo (target - x) $ filter (/= x) numbers
            return (x , y , z)

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
        readPassword (words -> [numbers , [letter , ':'] , pass]) =
            (Rule low high letter , pass)
            where
                (low , high) = (read *** read) $ second tail $ break (== '-') numbers

        satisfiesRuleMinMax :: (Rule , Password) -> Bool
        satisfiesRuleMinMax (Rule low high letter , pass) =
            low <= count && count <= high
            where
                count = length $ filter (== letter) pass

        satisfiesRulePositions :: (Rule , Password) -> Bool
        satisfiesRulePositions (Rule low high letter , pass) =
            length charsAtIndices == 1
            where
                charsAtIndices =
                    [ char
                    | (ix , char) <- zip [1 ..] pass
                    , ix == low || ix == high
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

{-
--- Day 4: Passport Processing ---

You arrive at the airport only to realize that you grabbed your North Pole
Credentials instead of your passport. While these documents are extremely
similar, North Pole Credentials aren't issued by a country and therefore aren't
actually valid documentation for travel in most of the world.

It seems like you're not the only one having problems, though; a very long line
has formed for the automatic passport scanners, and the delay could upset your
travel itinerary.

Due to some questionable network security, you realize you might be able to
solve both of these problems at the same time.

The automatic passport scanners are slow because they're having trouble
detecting which passports have all required fields. The expected fields are as
follows:

byr (Birth Year)
iyr (Issue Year)
eyr (Expiration Year)
hgt (Height)
hcl (Hair Color)
ecl (Eye Color)
pid (Passport ID)
cid (Country ID)

Passport data is validated in batch files (your puzzle input). Each passport is
represented as a sequence of key:value pairs separated by spaces or newlines.
    Passports are separated by blank lines.

Here is an example batch file containing four passports:

ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in

The first passport is valid - all eight fields are present. The second passport
is invalid - it is missing hgt (the Height field).

The third passport is interesting; the only missing field is cid, so it looks
like data from North Pole Credentials, not a passport at all! Surely, nobody
would mind if you made the system temporarily ignore missing cid fields. Treat
              this "passport" as valid.

The fourth passport is missing two fields, cid and byr. Missing cid is fine,
but missing any other field is not, so this passport is invalid.

According to the above rules, your improved system would report 2 valid
passports.

Count the number of valid passports - those that have all required fields.
    Treat cid as optional. In your batch file, how many passports are valid?

--- Part Two ---

The line is moving more quickly now, but you overhear airport security talking
about how passports with invalid data are getting through. Better add some data
validation, quick!

You can continue to ignore the cid field, but each other field has strict rules
about what values are valid for automatic validation:

byr (Birth Year) - four digits; at least 1920 and at most 2002.
iyr (Issue Year) - four digits; at least 2010 and at most 2020.
eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
hgt (Height) - a number followed by either cm or in:
If cm, the number must be at least 150 and at most 193.
If in, the number must be at least 59 and at most 76.
hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
pid (Passport ID) - a nine-digit number, including leading zeroes.
cid (Country ID) - ignored, missing or not.

Your job is to count the passports where all required fields are both present
and valid according to the above rules. Here are some example values:

byr valid:   2002
byr invalid: 2003

hgt valid:   60in
hgt valid:   190cm
hgt invalid: 190in
hgt invalid: 190

hcl valid:   #123abc
hcl invalid: #123abz
hcl invalid: 123abc

ecl valid:   brn
ecl invalid: wat

pid valid:   000000001
pid invalid: 0123456789
Here are some invalid passports:

eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007
Here are some valid passports:

pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719

Count the number of valid passports - those that have all required fields and
valid values. Continue to treat cid as optional. In your batch file, how many
passports are valid?
-}

data Credential = Credential
    { birthYear
    , issueYear
    , expirationYear
    , height
    , hairColor
    , eyeColor
    , passportId ::
          String
    , countryId :: Maybe String
    }

day4 :: IO ()
day4 = do
    credentials <- parseFile <$> readFile "puzzle-inputs/4.txt"
    putStrLn $
        unlines
            [ "Day 4"
            , "Valid passports " <> show (length credentials)
            , "Valid validated passports " <> show (length $ filter isValidCredential credentials)
            ]
    where
        parseFile :: String -> [Credential]
        parseFile = mapMaybe parseCredential . splitBy "" . lines

        splitBy :: Ord a => a -> [a] -> [[a]]
        splitBy separator l = foldr step [[]] l
            where
                step x (h : acc)
                    | x == separator = [] : h : acc
                    | otherwise = (x : h) : acc

        parseCredential :: [String] -> Maybe Credential
        parseCredential inp = do
            let fields = Map.fromList $ concatMap (fmap toKeyValue . words) inp
                at = (`Map.lookup` fields)
            birthYear <- at "byr"
            issueYear <- at "iyr"
            expirationYear <- at "eyr"
            height <- at "hgt"
            hairColor <- at "hcl"
            eyeColor <- at "ecl"
            passportId <- at "pid"
            let countryId = at "cid"
            return Credential {..}

        toKeyValue :: String -> (String , String)
        toKeyValue = (\[key , value] -> (key , value)) . splitBy ':'

        isValidCredential :: Credential -> Bool
        isValidCredential Credential {..} =
            all
                id
                [ length birthYear == 4
                , 1920 <= read birthYear && read birthYear <= 2002
                , length issueYear == 4
                , 2010 <= read issueYear && read issueYear <= 2020
                , length expirationYear == 4
                , 2020 <= read expirationYear && read expirationYear <= 2030
                , case span isDigit height of
                      (read -> n , "in") -> 59 <= n && n <= 76
                      (read -> n , "cm") -> 150 <= n && n <= 193
                      _ -> False
                , case hairColor of
                      '#' : hex -> length hex == 6 && all (`elem` hexNums) hex
                      _ -> False
                , eyeColor `Set.member` eyeColors
                , length passportId == 9 && all isDigit passportId
                ]
            where
                hexNums = ['0' .. '9'] ++ ['a' .. 'f']
                eyeColors = Set.fromList ["amb" , "blu" , "brn" , "gry" , "grn" , "hzl" , "oth"]
