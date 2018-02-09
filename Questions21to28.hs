module Questions21to28 where

    -- Questions 21 to 28 -  LISTS AGAIN

{-  Problem 21
Insert an element at a given position into a list.
Example in Haskell:
P21> insertAt 'X' "abcd" 2
"aXbcd" -}    

insertAt :: a -> [a] -> Int -> [a]
insertAt x [] _ = [x]
insertAt x l i
 | i < 1 || i > (length l) = error "Index out of bounds 1-length"
 | otherwise               = (take (i-1) l) ++ [x] ++ (drop (i-1) l)

{- Problem 22
Create a list containing all integers within a given range.
Example in Haskell:
Prelude> range 4 9
[4,5,6,7,8,9] -}

range :: Int -> Int -> [Int]
range i j = [i..j]