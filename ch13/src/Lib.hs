module Lib
    ( module Lib,
      module Control.Applicative
    ) where

import Control.Applicative
import Data.Char
import System.IO

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- type Parser a = String -> [(a, String)]
newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
            [] -> []
            (x:xs) -> [(x, xs)])

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = P (\inp -> case parse p inp of
                    [] -> []
                    [(v, out)] -> [(g v, out)])

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure v = P (\inp -> [(v, inp)])

    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P (\inp -> case parse pg inp of
                     [] -> []
                     [(g, out)] -> parse (fmap g px) out)

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\inp -> case parse p inp of
                   [] -> []
                   [(v, out)] -> parse (f v) out)

{- class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a -}

instance Alternative Parser where
    -- empty :: Parser a
    empty =  P (\inp -> [])

    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\inp -> case parse p inp of
                   [] -> parse q inp
                   [(v, out)] -> [(v, out)])

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (==x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

space :: Parser ()
space = do many (sat isSpace)
           return ()

int :: Parser Int
int  = do char '-'
          n <- nat
          return (-n)
      <|> nat

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

nats :: Parser [Int]
nats = do symbol "["
          n <- natural
          ns <- many (do symbol ","
                         natural)
          symbol "]"
          return (n:ns)


-- arithmetic grammer (right associate)
{- 
    expr   ::= term (+ expr | "")
    term   ::= factor (* term | "")
    factor ::= ( expr ) | nat
    nat    ::= 0 | 1 | 2 | ...
-}

expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
           <|> return t

term :: Parser Int
term = do f <- factor
          do symbol "*"
             t <- term
             return (f * t)
           <|> return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
         <|> natural

eval :: String -> Int
eval ns = case (parse expr ns) of
              [(n, [])]  -> n
              [(_, out)] -> error ("Unused input " ++ out)
              []         -> error "Invalid input"


-- calculator
getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

beep :: IO ()
beep = putStr "\BEL"

cls :: IO ()
cls = putStr "\ESC[2J"

writeat :: (Int, Int) -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

goto :: (Int, Int) -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")


box :: [String]
box = ["+---------------+",
       "|               |",
       "+---+---+---+---+",
       "| q | c | d | = |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]

buttons :: String
buttons = standard ++ extra
    where
        standard = "acd=123+456-789*0()/"
        extra    = "QCD \ESC\BS\DEL\n"

showbox :: IO ()
showbox = sequence_ [writeat (1, y) b | (y, b) <- zip [1..] box]

display xs = do writeat (3, 2) (replicate 13 ' ')
                writeat (3, 2) (reverse (take 13 (reverse xs)))

calc :: String -> IO ()
calc xs = do display xs
             c <- getCh
             if elem c buttons then
                process c xs
             else
                do beep
                   calc xs

process :: Char -> String -> IO ()
process c xs | elem c "qQ\ESC"    = quit
             | elem c "dD\BS\DEL" = delete xs
             | elem c "=\n"       = eval' xs
             | elem c "cC"        = clear
             | otherwise          = press c xs

quit :: IO ()
quit = goto (1, 14)

delete :: String -> IO ()
delete [] = calc []
delete xs = calc (init xs)

eval' :: String -> IO ()
eval' xs = case parse expr xs of
               [(n, [])] -> calc (show n)
               _         -> do beep
                               calc xs

clear :: IO ()
clear = calc []

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do cls
         showbox
         clear


-- 13.11Exercises

{- 
    1.Define a parser comment :: Parser () for ordinary Haskell comments that begin 
    with the symbol -- and extend to the end of the current line, which is represented 
    by the control character ’\n’.
-}
comment :: Parser ()
comment = do symbol "--"
             many (sat (/= '\n'))
             return ()

{- 
    6.Extend the parser expr :: Parser Int to support subtraction and division, and to 
    use integer values rather than natural numbers, based upon the following revisions 
    to the grammar:
    expr'   ::= term' (+ expr' | - expr' | "")
    term'   ::= expo' (* term' | / term' | "")
    expo'   ::= factor' (^ expo' | "")
    factor' ::= ( expr' ) | int
    int     ::= ... | -1 | 0 | 1 | ...

    7.Further extend the grammar and parser for arithmetic expressions to support exponentiation ^, 
    which is assumed to associate to the right and have higher priority than multiplication and division, 
    but lower priority than parentheses and numbers. For example, 2^3*4 means (2^3)*4. Hint: the new 
    level of priority requires a new rule in the grammar.
-}
expr' :: Parser Int
expr' = do t <- term'
           do symbol "+"
              e <- expr'
              return (t+e)
            <|> do symbol "-"
                   e <- expr'
                   return (t-e)
            <|> return t

term' :: Parser Int
term' = do e <- expo'
           do symbol "*"
              t <- term'
              return (e*t)
            <|> do symbol "/"
                   t <- term'
                   return (e `div` t)
            <|> return e

expo' :: Parser Int
expo' = do f <- factor'
           do symbol "^"
              e <- expo'
              return (f^e)
            <|> return f

factor' :: Parser Int
factor' = do symbol "("
             e <- expr'
             symbol ")"
             return e
            <|> int
