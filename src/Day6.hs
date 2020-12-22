module Day6 where

import qualified Data.Set as Set

main :: IO ()
main = do
    groups <- toGroups <$> readFile "puzzle-inputs/6.txt"
    putStrLn $
        unlines
            [ "Day 6"
            , "Sum of yes answers from anyone: " <> show (sum $ map anyAnsweredYes groups)
            , "Sum of yes answers from everyone: " <> show (sum $ map allAnsweredYes groups)
            ]

toGroups :: String -> [[String]]
toGroups = splitOn "" . lines

anyAnsweredYes :: [String] -> Int
anyAnsweredYes = Set.size . Set.fromList . concat

allAnsweredYes :: [String] -> Int
allAnsweredYes = Set.size . foldr1 Set.intersection . map Set.fromList

splitOn :: Ord a => a -> [a] -> [[a]]
splitOn separator l = foldr step [[]] l
    where
        step x (h : acc)
            | x == separator = [] : h : acc
            | otherwise = (x : h) : acc
