{-# LANGUAGE TupleSections #-}

module Day7 where

import Data.Bifunctor (first, second)
import Data.Char (isNumber)
import Data.List (uncons)
import Data.Map.Strict (Map)
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

main :: IO ()
main = do
    graph <- toGraph . lines <$> readFile "puzzle-inputs/7.txt"

    let lookupDef def m k = fromMaybe def $ Map.lookup k m

        canContainShinyGold = reachable parentsOf "shiny gold"
            where
                parentsOf = lookupDef [] (invert graph)

    putStrLn $
        unlines
            [ "Day 7"
            , "Bags that can contain a shiny gold bag: " <> show (Set.size canContainShinyGold)
            , "Bags inside a shiny gold bag: " <> show (weightedDepth (lookupDef [] graph) "shiny gold")
            ]

type Color = String

toGraph :: [String] -> Map Color [(Int , Color)]
toGraph = Map.fromListWith (<>) . map asEdge

asEdge :: String -> (String , [(Int , String)])
asEdge =
    first unwords
        . second (map withNumber)
        . fromJust
        . uncons
        . split (all isNumber)
        . filter (not . flip elem ["contain" , "bag" , "bags" , "no other bags"])
        . words
        . filter (not . flip elem ['.' , ','])
    where
        withNumber (n : ws) = (read n , unwords ws)

        split pred = reverse . map reverse . go [[]]
            where
                go (curr : prev) [] = curr : prev
                go (curr : prev) (h : t)
                    | pred h = go ([h] : curr : prev) t
                    | otherwise = go ((h : curr) : prev) t

invert :: Map Color [(Int , Color)] -> Map Color [Color]
invert graph =
    Map.fromListWith
        (<>)
        [ (child , [parent])
        | (parent , children) <- Map.toList graph
        , (_ , child) <- children
        ]

reachable :: Ord a => (a -> [a]) -> a -> Set a
reachable children root = foldr step mempty (children root)
    where
        step node visited
            | node `Set.member` visited = visited
            | otherwise = foldr step (Set.insert node visited) $ children node

weightedDepth :: Ord a => (a -> [(Int , a)]) -> a -> Int
weightedDepth children = (\x -> x - 1) . snd . go mempty
    where
        go calculated root = foldr step (calculated , 1) (children root)

        step (amount , node) (calculated , total) = case Map.lookup node calculated of
            Just val -> (calculated , total + amount * val)
            Nothing ->
                let (calculated' , val) = go calculated node
                 in (Map.insert node val calculated' , amount * val + total)
