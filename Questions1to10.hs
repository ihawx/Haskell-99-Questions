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