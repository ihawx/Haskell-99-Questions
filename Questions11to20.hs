module Questions11to20 where

-- Questions 11 to 20 -  LISTS, CONTINUED

{- Problem 11
(*) Modified run-length encoding.
Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
Example in Haskell:
P11> encodeModified "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e'] -}

data MultiOrSingle a = Multiple Int a | Single a deriving (Show)

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack x = let h = head x
         in (takeWhile (==h) x) : (pack (dropWhile (==h) x))

encode :: (Eq a) => [a] -> [(Int,a)]
encode x = let packeged = pack x
           in map (\l -> (length l,head l)) packeged

encodeModified :: (Eq a) => [a] -> [MultiOrSingle a]
encodeModified [] = []
encodeModified x = let encoded = encode x
                   in map (\(n, e) -> if n>1 then (Multiple n e) else (Single e)) encoded

{- Problem 12
(**) Decode a run-length encoded list.
Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
Example in Haskell:
P12> decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
"aaaabccaadeeee" -}

decodeModified :: [MultiOrSingle a] -> [a]
decodeModified [] = []
decodeModified ((Single x):xs) = [x]++decodeModified xs
decodeModified ((Multiple n e):xs) 
 | n>1 = [e] ++ (decodeModified ((Multiple (n-1) e):xs))
 | n == 1 = [e] ++ (decodeModified xs)

{- Problem 13
(**) Run-length encoding of a list (direct solution).
Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
Example in Haskell:
P13> encodeDirect "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e'] -}

encodeDirect :: (Eq a) => [a] -> [MultiOrSingle a]
encodeDirect [] = []
encodeDirect (x:xs) = let count = length (takeWhile (==x) (x:xs))
                          rest = dropWhile (==x) (xs)
                      in if count == 1 then ([Single x] ++ (encodeDirect rest)) else ([Multiple count x] ++ (encodeDirect rest))

{- Problem 14
(*) Duplicate the elements of a list.
Example in Haskell:
> dupli [1, 2, 3]
[1,1,2,2,3,3] -}                     

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = [x,x]++dupli xs

{- Problem 15
(**) Replicate the elements of a list a given number of times.
Example in Haskell:
> repli "abc" 3
"aaabbbccc" -}

repli :: [a] -> Int -> [a]
repli [] _ = []
repli x 0 = x
repli (x:xs) n = (replicate n x) ++ (repli xs n)


{- Problem 16
(**) Drop every N'th element from a list.
Example in Haskell:
*Main> dropEvery "abcdefghik" 3
"abdeghk" -}

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery x n
 | n > (length x) = x
 | n < 1 = x
 | otherwise = (take (n-1) x) ++ (dropEvery (drop n x) n)

{- Problem 17
(*) Split a list into two parts; the length of the first part is given.
Do not use any predefined predicates.
Example in Haskell:
*Main> split "abcdefghik" 3
("abc", "defghik") -}

split :: [a] -> Int -> ([a],[a])
split [] _ = ([],[])
split x n
 |n<1 = ([],x)
 |n>((length x)-1) = (x,[])
 |otherwise = (take n x, drop n x)

{- Problem 18
(**) Extract a slice from a list.
Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 1.
Example in Haskell:
*Main> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
"cdefg" -}

slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice x a b
 |a < 1 = x
 |b > (length x) = x
 |a > b = []
 |a == b = [x !! (a-1)]
 |otherwise = take (b-a+1) $ drop (a-1) x

{-  Problem 19

(**) Rotate a list N places to the left.
Hint: Use the predefined functions length and (++).
Examples in Haskell:
*Main> rotate ['a','b','c','d','e','f','g','h'] 3
"defghabc" 
*Main> rotate ['a','b','c','d','e','f','g','h'] (-2)
"ghabcdef" -}

rotate :: [a] -> Int -> [a]
rotate x 0 = x
rotate [] _ = []
rotate x n
 |n>0 = let l = length x
            rots = n `mod` l
        in (drop rots x) ++ (take rots x)
 |n<0 = let l = length x
            rots = l - ((-n) `mod` l)
        in (drop rots x) ++ (take rots x)

{-  10 Problem 20
(*) Remove the K'th element from a list.
Example in Haskell:
*Main> removeAt 2 "abcd"
('b',"acd") -}        

removeAt :: Int -> [a] -> (a,[a])
removeAt _ [] = error "Cant remove from empty list"
removeAt n x
 | n < 1 || n > (length x) = error "Index out of bounds 1 - length"
 | otherwise = (x!!(n-1),(take (n-1) x) ++ (drop n x))
