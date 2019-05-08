module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]


positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']

count :: Char -> String -> Int
count x xs = sum [1 | x' <- xs, x == x']

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

decode :: Int -> String -> String
decode n = encode (-n)

lowers :: String -> Int
lowers xs = length [x | x <- xs, isAsciiLower x]

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 
  4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
        where n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = decode factor xs
    where
        factor = head (positions (minimum chitab) chitab)
        chitab = [chisqr (rotate n table') table | n <- [0..25]]
        table' = freqs xs


--5.7Exercises

{- 1.Using a list comprehension, give an expression that calculates the 
    sum 1^2 + 2^2 + ... 100^2 of the first one hundred integer squares. 
-}
sumsquare :: Int
sumsquare = sum [x^2 | x <- [1..100]]

{- 2.Suppose that a coordinate grid of size m × n is given by the list of all 
    pairs (x, y) of integers such that  Using a list comprehension, define a 
    function grid :: Int -> Int -> [(Int,Int)] that returns a coordinate grid 
    of a given size. For example:
    > grid 1 2
    [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)] 
-}
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]]

{- 3.Using a list comprehension and the function grid above, define a function 
    square :: Int -> [(Int,Int)] that returns a coordinate square of size n, 
    excluding the diagonal from (0, 0) to (n, n). For example:
    > square 2
    [(0,1),(0,2),(1,0),(1,2),(2,0),(2,1)] 
-}
square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

{- 4.In a similar way to the function length, show how the library function 
    replicate :: Int -> a -> [a] that produces a list of identical elements 
    can be defined using a list comprehension. For example:
    > replicate 3 True
    [True,True,True]
-}
replicate :: Int -> a -> [a]
replicate n a = [a | _ <- [1..n]]

{- 5.A triple (x, y, z) of positive integers is Pythagorean if it satisfies 
    the equation x^2 + y^2 = z^2. Using a list comprehension with three generators, 
    define a function pyths :: Int -> [(Int,Int,Int)] that returns the list of all 
    such triples whose components are at most a given limit. For example:
    > pyths 10
    [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
-}
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

{- 6.A positive integer is perfect if it equals the sum of all of its factors, excluding 
    the number itself. Using a list comprehension and the function factors, define a 
    function perfects :: Int -> [Int] that returns the list of all perfect numbers up to 
    a given limit. For example:
    > perfects 500
    [6,28,496]
-}
perfect :: Int -> Bool
perfect n = sum (init (factors n)) == n

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], perfect x]


{- 7.Show how the list comprehension [(x,y) | x <- [1,2], y <- [3,4]] with two generators 
    can be re-expressed using two comprehensions with single generators. Hint: nest one 
    comprehension within the other and make use of the library function 
    concat :: [[a]] -> [a].
-}
comp :: [(Int,Int)]
comp = concat [[(a,b) | a <- [1..2]] | b <- [3..4]]


{- 
    8.Redefine the function positions using the function find.
-}

positions2 :: Eq a => a -> [a] -> [Int]
positions2 n xs = find n (zip xs [0..(length xs - 1)])

{- 
    9.The scalar product of two lists of integers xs and ys of length n is given by 
    the sum of the products of corresponding integers:
        sum (xsi*ysi) where i <- [0..n-1]
    In a similar manner to chisqr, show how a list comprehension can be used to define 
    a function scalarproduct :: [Int] -> [Int] -> Int that returns the scalar product 
    of two lists. For example:
    > scalarproduct [1,2,3] [4,5,6]
    32
-}
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x, y) <- zip xs ys]
