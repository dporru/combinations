import Control.Monad
import System.IO
import Data.Char

main = do
    -- let first_block = possible_combinations 953 5520 3
    -- print first_block
    handle <- openFile "woorden.max" ReadMode  
    contents <- hGetContents handle  
    print $ length $ hasA $ filterWords $ woordenlijst contents 
    hClose handle
    
    
woordenlijst :: String -> [String]
woordenlijst content = map filterChars (lines content)

hasA :: [String] -> [String]
hasA [] = []
hasA (x:xs)
    | x!!3 == 'a' = x:hasA xs
	| otherwise   = hasA xs

filterWords :: [String] -> [String]
filterWords [] = []
filterWords (x:xs)
    | length x == 8 = x:filterWords xs
    | otherwise     = filterWords xs

filterChars :: [Char] -> [Char]
filterChars []   = []
filterChars (x:xs) = if (isLetter x) then (x:(filterChars xs)) else (filterChars xs)

letters = zip ['a'..'z'] [1..]

norm :: Int -> Int -> Int
norm noemer x = noemer `div` x

possible_letters teller noemer = [ (l,n) | (l,n) <- letters, noemer `mod` n == 0, noemer `div` n < teller ]
 
possible_combinations :: Int -> Int -> Int -> [String]
possible_combinations teller noemer positions = [ get_string x | x <- combinations, comb_sum x < teller]
    where combinations = replicateM positions $ possible_letters teller noemer
          comb_sum     = foldl (\c (_,x) -> c + x) 0
          get_string   = foldr (\(a,_) c -> a:c) ""
