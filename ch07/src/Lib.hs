module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

_map :: (a -> b) -> [a] -> [b]
_map f xs = [f x | x <- xs]

--transmit
type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concatMap (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id
--transmit


--first past the post
votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x) 

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

result :: Ord a => [a] -> [(Int, a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

winner :: Ord a => [a] -> a
winner = snd . last . result
--first past the post


--7.9Exercises
{- 
    4.Using foldl, define a function dec2int :: [Int] -> Int that converts 
    a decimal number into an integer. For example:
    > dec2int [2,3,4,5]
    2345
-}
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> x * 10 + y) 0

{- 
    6.A higher-order function unfold that encapsulates a simple pattern of recursion 
    for producing a list can be defined as follows:
    unfold p h t x | p x       = []
                   | otherwise = h x : unfold p h t (t x)
    That is, the function unfold p h t produces the empty list if the predicate p is 
    true of the argument value, and otherwise produces a non-empty list by applying 
    the function h to this value to give the head, and the function t to generate 
    another argument that is recursively processed in the same way to produce the 
    tail of the list. For example, the function int2bin can be rewritten more compactly 
    using unfold as follows:
    int2bin = unfold (== 0) (‘mod‘ 2) (‘div‘ 2)
    Redefine the functions chop8, map f and iterate f using unfold.

    chop8 = unfold (== []) (take 8) (drop 8)
-}


{- 
    9.Define a function altMap :: (a -> b) -> (a -> b) -> [a] -> [b] that alternately 
    applies its two argument functions to successive elements in a list, in turn about 
    order. For example:
    > altMap (+10) (+100) [0,1,2,3,4]
    [10,101,12,103,14]
-}
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x:xs) = f x : altMap g f xs


{- 
    10.Using altMap, define a function luhn :: [Int] -> Bool that implements the 
    Luhn algorithm from the exercises in chapter 4 for bank card numbers of any 
    length. Test your new function using your own bank card.
-}
luhnDouble :: Int -> Int
luhnDouble n | dn > 9    = dn - 9
             | otherwise = dn
             where dn = n * 2

luhn :: [Int] -> Bool
luhn xs = sum (altMap luhnDouble id xs) `mod` 10 == 0
