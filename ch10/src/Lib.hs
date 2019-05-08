module Lib
    ( someFunc,
      hangman,
      nim,
      playLife,
      putStr',
      adder
    ) where

import System.IO (hSetEcho, stdin)
import Data.Char (isDigit, digitToInt)

someFunc :: IO ()
someFunc = putStrLn "someFunc"



-- 10.6Hangman
hangman :: IO ()
hangman = do putStrLn "Think of a word:"
             word <- sgetLine
             putStrLn "Try to guess it:"
             play word

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                do putChar x
                   return []
              else
                do putChar '-'
                   xs <- sgetLine
                   return (x:xs)

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

play :: String -> IO ()
play word = do putStr "? "
               guess <- getLine
               if guess == word then
                putStrLn "You got it!"
               else 
                do putStrLn (match word guess)
                   play word 

match :: String -> String -> String
match xs ys = [if elem x ys then x else '-' | x <- xs]


--10.7Nim
next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished = all (==0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row - 1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r, n) <- zip [1..] board]
                     where
                      update r n  = if r == row then n - num else n

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard :: Board -> Int -> IO ()
putBoard [] _ = do putChar ' '
putBoard (x:xs) n = do putRow n x
                       putBoard xs (n+1)

getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     newline
                     if isDigit x then
                       return (digitToInt x)
                     else
                       do putStrLn "ERROR: Invalid digit"
                          getDigit prompt

newline :: IO ()
newline = do putChar '\n'

play' :: Board -> Int -> IO ()
play' board player = 
  do newline
     putBoard board 1
     if finished board then
      do newline
         putStr "Player "
         putStr (show (next player))
         putStrLn "Win!"
     else
      do newline
         putStr "Player "
         putStrLn (show player)
         row <- getDigit "Enter a row number: "
         num <- getDigit "Stars to remove: "
         if valid board row num then
          play' (move board row num) (next player)
         else
          do newline
             putStrLn "ERROR: Invalid move"
             play' board player

nim :: IO ()
nim = play' initial 1


--10.8Life

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

width :: Int
width = 10

height :: Int
height = 10

type Board' = [Pos]

glider :: Board'
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

showcells :: Board' -> IO ()
showcells b = sequence_ [writeat p "O" | p <- b]

isAlive :: Board' -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board' -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1, y-1), (x, y-1), 
                          (x+1, y-1), (x-1, y), 
                          (x+1, y), (x-1, y+1), 
                          (x, y+1), (x+1, y+1)]

wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width) + 1, ((y-1) `mod` height) + 1)

liveneighbs :: Board' -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board' -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2,3]]

births :: Board' -> [Pos]
births b = [p | p <- rmdups (concat (map neighbs b)),
                isEmpty b p,
                liveneighbs b p == 3]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

nextgen :: Board' -> Board'
nextgen b = survivors b ++ births b

life :: Board' -> IO ()
life b = do cls
            showcells b
            wait 500000
            life (nextgen b)

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]

playLife :: IO ()
playLife = life glider



--10.10Exercises

{- 
   1.Redefine putStr :: String -> IO () using a list comprehension and the 
   library function sequence_ :: [IO a] -> IO ().
-}
putStr' :: String -> IO ()
putStr' xs = sequence_ [putChar x | x <- xs]

{- 
   3.In a similar manner to the first exercise, redefine the generalised version 
   of putBoard using a list comprehension and sequence_.
-}
putBoard' :: Board -> IO ()
putBoard' xs = sequence_ [putRow r x | (r, x) <- zip [1..] xs]

{- 
   4.Define an action adder :: IO () that reads a given number of integers from the 
   keyboard, one per line, and displays their sum. For example:
   > adder
   How many numbers? 5
   1
   3
   5
   7
   9
   The total is 25
   Hint: start by defining an auxiliary function that takes the current total and 
   how many numbers remain to be read as arguments. You will also likely need to use 
   the library functions read and show.
-}
adder :: IO ()
adder = do putStr "How many numbers?"
           cnt <- getLine
           sum <- getSum (read cnt::Int) 0
           putStr "The total is "
           putStrLn (show sum)

getSum :: Int -> Int -> IO Int
getSum cnt sum = do if cnt == 0 then
                      return sum
                    else
                      do x <- getLine
                         let n = read x::Int
                         getSum (cnt-1) (sum + n)
