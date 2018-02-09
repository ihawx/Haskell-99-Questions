module Questions1to10 where

-- Questions 1 to 10 -  LISTS

{- Problem 1
(*) Find the last element of a list.
Example in Haskell:
Prelude> myLast [1,2,3,4]
4
Prelude> myLast ['x','y','z']
'z' -}

myLast :: [a] -> a
myLast = head . reverse

{- Problem 2
(*) Find the last but one element of a list.
Example in Haskell:
Prelude> myButLast [1,2,3,4]
3
Prelude> myButLast ['a'..'z']
'y'  -}

myButLast :: [a] -> a
myButLast = last . init


{- Problem 3
(*) Find the K'th element of a list. The first element in the list is number 1.
Example in Haskell:
Prelude> elementAt [1,2,3] 2
2
Prelude> elementAt "haskell" 5
'e' -}

elementAt :: [a] -> Int -> a
elementAt [] _ = error "Cant access empty list"
elementAt l n
    | n<1 || n>(length l) = error "Index out of bounds"
    | otherwise = l !! (n-1)


{- Problem 4
(*) Find the number of elements of a list.
Example in Haskell:
Prelude> myLength [123, 456, 789]
3
Prelude> myLength "Hello, world!"
13 -}  

myLength :: (Num b) => [a] -> b
myLength = foldr (\_ acc -> acc+1) 0


{- Problem 5
Reverse a list.
Example in Haskell:
Prelude> myReverse "A man, a plan, a canal, panama!"
"!amanap ,lanac a ,nalp a ,nam A"
Prelude> myReverse [1,2,3,4]
[4,3,2,1] -}

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]


{- Problem 6
Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
Example in Haskell:
*Main> isPalindrome [1,2,3]
False
*Main> isPalindrome "madamimadam"
True
*Main> isPalindrome [1,2,4,8,16,8,4,2,1]
True -}

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = False
isPalindrome x = x == (reverse x)


{- Problem 7
(**) Flatten a nested list structure.
Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
Example in Haskell:
We have to define a new data type, because lists in Haskell are homogeneous.

        data NestedList a = Elem a | List [NestedList a]

*Main> flatten (Elem 5)
[5]
*Main> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
[1,2,3,4,5]
*Main> flatten (List [])
[] -}

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

{-  Problem 8
(**) Eliminate consecutive duplicates of list elements.
If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
Example in Haskell:
> compress "aaaabccaadeeee"
"abcade" -}

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) = x : (compress $ dropWhile (==x) xs)


{- Problem 9
(**) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
Example in Haskell:
*Main> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
["aaaa","b","cc","aa","d","eeee"] -}

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack x = let h = head x
         in (takeWhile (==h) x) : (pack (dropWhile (==h) x))


{- Problem 10
(*) Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
Example in Haskell:
encode "aaaabccaadeeee"
[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')] -}    

encode :: (Eq a) => [a] -> [(Int,a)]
encode x = let packeged = pack x
           in map (\l -> (length l,head l)) packeged