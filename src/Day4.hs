{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Day4 where

import Data.Char (isDigit)
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map
import qualified Data.Set as Set

data Credential = Credential
    { birthYear , issueYear , expirationYear , height , hairColor , eyeColor , passportId :: String
    , countryId :: Maybe String
    }

main :: IO ()
main = do
    credentials <- parseFile <$> readFile "puzzle-inputs/4.txt"
    putStrLn $
        unlines
            [ "Day 4"
            , "Valid passports " <> show (length credentials)
            , "Valid validated passports " <> show (length $ filter isValidCredential credentials)
            ]

parseFile :: String -> [Credential]
parseFile = mapMaybe parseCredential . splitOn mempty . lines

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
        [ length birthYear == 4 && 1920 <= read @Int birthYear && read @Int birthYear <= 2002
        , length issueYear == 4 && 2010 <= read @Int issueYear && read @Int issueYear <= 2020
        , length expirationYear == 4 && 2020 <= read @Int expirationYear && read @Int expirationYear <= 2030
        , case span isDigit height of
              (read @Int -> n , "in") -> 59 <= n && n <= 76
              (read @Int -> n , "cm") -> 150 <= n && n <= 193
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

splitOn :: Ord a => a -> [a] -> [[a]]
splitOn separator l = foldr step [[]] l
    where
        step x (h : acc)
            | x == separator = [] : h : acc
            | otherwise = (x : h) : acc
