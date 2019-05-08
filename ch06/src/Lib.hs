module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y    = x : y : ys
                | otherwise = y : insert x ys

insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where 
                smaller = [a | a <- xs, a <= x]
                larger  = [b | b <- xs, b > x]


-- 6.8Exercises

{- 
    2.Define a recursive function sumdown :: Int -> Int that returns 
    the sum of the non-negative integers from a given value down to 
    zero. For example, sumdown 3 should return the result 3+2+1+0 = 6.
 -}      
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

{- 
    3.Define the exponentiation operator ^ for non-negative integers using 
    the same pattern of recursion as the multiplication operator *, and show 
    how the expression 2 ^ 3 is evaluated using your definition.
 -}
expo :: Int -> Int -> Int
expo _ 0 = 1
expo base e = base * expo base (e-1)

{- 
    4.Define a recursive function euclid :: Int -> Int -> Int that implements 
    Euclid’s algorithm for calculating the greatest common divisor of two non-negative 
    integers: if the two numbers are equal, this number is the result; otherwise, the 
    smaller number is subtracted from the larger, and the same process is then repeated. 
    For example:
    > euclid 6 27
    3
 -}
euclid :: Int -> Int -> Int
euclid a b | a == b    = a
           | a > b     = euclid b a
           | otherwise = euclid a (b-a)

{- 
    6.Without looking at the definitions from the standard prelude, define the 
    following library functions on lists using recursion.
    a.Decide if all logical values in a list are True:
    and :: [Bool] -> Bool
    b.Concatenate a list of lists:
    concat :: [[a]] -> [a]
    c.Produce a list with n identical elements:
    replicate :: Int -> a -> [a]
    d.Select the nth element of a list:
    (!!) :: [a] -> Int -> a
    e.Decide if a value is an element of a list:
    elem :: Eq a => a -> [a] -> Bool
    Note: most of these functions are defined in the prelude using other library 
    functions rather than using explicit recursion, and are generic functions rather 
    than being specific to the type of lists.
 -}
_and :: [Bool] -> Bool
_and [] = True
_and (x:xs) = x && and xs

_concat :: [[a]] -> [a]
_concat [xs] = xs
_concat (x:xs) = x ++ _concat xs

_replicate :: Int -> a -> [a]
_replicate 0 _ = []
_replicate n a = a : _replicate (n-1) a

(!!!) :: [a] -> Int -> a
(!!!) (x:xs) 0 = x
(!!!) (x:xs) n = (!!!) xs (n-1)

_elem :: Eq a => a -> [a] -> Bool
_elem a [] = False
_elem a (b:bs) | a == b    = True
               | otherwise = _elem a bs

{- 
    7.Define a recursive function merge :: Ord a => [a] -> [a] -> [a] 
    that merges two sorted lists to give a single sorted list. For example:
    > merge [2,5,6] [1,3,4]
    [1,2,3,4,5,6]
    Note: your definition should not use other functions on sorted lists 
    such as insert or isort, but should be defined using explicit recursion.
 -}
merge ::Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x <= y    = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

{- 
    8.Using merge, define a function msort :: Ord a => [a] -> [a] that implements 
    merge sort, in which the empty list and singleton lists are already sorted, 
    and any other list is sorted by merging together the two lists that result from 
    sorting the two halves of the list separately.
    Hint: first define a function halve :: [a] -> ([a],[a]) that splits a list into 
    two halves whose lengths differ by at most one.
 -}
halve :: [a] -> ([a],[a])
halve xs = (take m xs, drop m xs)
            where m = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort f) (msort s)
           where
            (f, s) = halve xs
			