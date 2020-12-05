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
    day5

-- Day 1

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

-- Day 3

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

-- Day 3

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

-- Day 4

data Credential = Credential
    { birthYear , issueYear , expirationYear , height , hairColor , eyeColor , passportId :: String
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
        parseFile = mapMaybe parseCredential . splitOn empty . lines

        splitOn :: Ord a => a -> [a] -> [[a]]
        splitOn separator l = foldr step [[]] l
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
        toKeyValue = (\[key , value] -> (key , value)) . splitOn ':'

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

-- Day 5

data Bit = One | Zero

day5 :: IO ()
day5 = do
    seatIds <- map (seatId . codeToRowColumn) . lines <$> readFile "puzzle-inputs/5.txt"
    putStrLn $
        unlines
            [ "Day 5"
            , "Highest seat id " <> show (maximum seatIds)
            , "My seat number is " <> show (mySeatNumber seatIds)
            ]
    where
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
