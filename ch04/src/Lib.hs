module Lib
    ( someFunc, halve
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


halve :: [a] -> ([a], [a])
halve xs = (take mid xs, drop mid xs)
        where mid = (length xs) `div` 2


third :: [a] -> a
-- third xs = head(tail (tail xs))

-- third xs = xs !! 2

third (_:_:x:_) = x


safetail :: [a] -> [a]
-- safetail xs = if null xs then xs else tail xs

-- safetail xs | null xs    = xs
--             | otherwise  = tail xs

safetail [] = []
safetail (_:xs) = xs

(||) :: Bool -> Bool -> Bool
-- False || False = False
-- False || True  = True
-- True  || False = True
-- True  || True  = True

-- False || False = False
-- _     || _     = True

-- False || b = b
-- True  || _ = True

b || c | b == c    = b
       | otherwise = True


mult :: Int -> Int -> Int -> Int
-- mult x y z = x*y*z
mult = \x -> (\y -> (\z -> x*y*z))


luhnDouble :: Int -> Int
luhnDouble n | dn > 9    = dn - 9
             | otherwise = dn
             where dn = n * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = ((luhnDouble a) + b + (luhnDouble c) + d) `rem` 10 == 0