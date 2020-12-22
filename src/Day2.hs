{-# LANGUAGE ViewPatterns #-}

module Day2 where

import Control.Arrow ((***))
import Data.Bifunctor (second)

data Rule = Rule Int Int Char

type Password = String

main :: IO ()
main = do
    passwords <- fmap readPassword . lines <$> readFile "puzzle-inputs/2.txt"
    putStrLn $
        unlines
            [ "Day 2"
            , "Passwords that satisfy the rules for min and max: "
                  <> show (length $ filter satisfiesRuleMinMax passwords)
            , "Passwords that satisfy the rules for index positions: "
                  <> show (length $ filter satisfiesRulePositions passwords)
            ]

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
