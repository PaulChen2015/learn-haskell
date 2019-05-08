module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Nat = Zero | Succ Nat
           deriving (Show)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ(int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)


-- 8.9Exercises

{- 
    1.In a similar manner to the function add, define a recursive multiplication 
    function mult :: Nat -> Nat -> Nat for the recursive type of natural numbers:
    Hint: make use of add in your definition.
-}
mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult _ Zero = Zero
mult (Succ m) n = add n (mult m n)

{- 
    2.Although not included in appendix B, the standard prelude defines
    data Ordering = LT | EQ | GT
    together with a function
    compare :: Ord a => a -> a -> Ordering
    that decides if one value in an ordered type is less than (LT), equal to (EQ), 
    or greater than (GT) another value. Using this function, redefine the function 
    occurs :: Ord a => a -> Tree a -> Bool for search trees. Why is this new definition 
    more efficient than the original version?
-}
data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Node l y r) = case compare x y of
                            LT -> occurs x l
                            EQ -> True
                            GT -> occurs x r

{- 
    3.Consider the following type of binary trees:
    data Tree a = Leaf a | Node (Tree a) (Tree a)
    Let us say that such a tree is balanced if the number of leaves in the left and right 
    subtree of every node differs by at most one, with leaves themselves being trivially 
    balanced. Define a function balanced :: Tree a -> Bool that decides if a binary tree 
    is balanced or not.
    Hint: first define a function that returns the number of leaves in a tree.
-}
data Tree2 a = Leaf2 a | Node2 (Tree2 a) (Tree2 a)
leaves :: Tree2 a -> Int
leaves (Leaf2 _) = 1
leaves (Node2 l r) = leaves l + leaves r

balanced :: Tree2 a -> Bool
balanced (Leaf2 _) = True
balanced (Node2 l r) = abs (leaves l - leaves r) <= 1 && balanced l && balanced r

{- 
    4.Define a function balance :: [a] -> Tree a that converts a non-empty list 
    into a balanced tree. Hint: first define a function that splits a list into 
    two halves whose length differs by at most one.
-}
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

balance :: [a] -> Tree2 a
balance [a] = Leaf2 a
balance xs = Node2 (balance l) (balance r)
             where (l, r) = halve xs

{- 
    5.Given the type declaration
    data Expr = Val Int | Add Expr Expr
    define a higher-order function
    folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a such that folde f g replaces 
    each Val constructor in an expression by the function f, and each Add constructor 
    by the function g.
-}
data Expr = Val Int | Add Expr Expr | Mult Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val a) = f a
folde f g (Add a b) = g (folde f g a) (folde f g b)


{- 
    6.Using folde, define a function eval :: Expr -> Int that evaluates an 
    expression to an integer value, and a function size :: Expr -> Int that 
    calculates the number of values in an expression.
-}
eval :: Expr -> Int
eval = folde id (+)

expr_ :: Expr
expr_ = Add (Val 1) (Val 2)

size :: Expr -> Int
size = folde (\_ -> 1) (+)

{- 
    9.Extend the abstract machine to support the use of multiplication.
-}
-- value :: Expr -> Int
-- value (Val n) = n
-- value (Add x y) = value x + value y

type Cont = [Op]

data Op = ADD Expr | MULT Expr | PLUS Int | TIMES Int

eval' :: Expr -> Cont -> Int
eval' (Val n)   c = exec c n
eval' (Add x y) c = eval' x (ADD y : c)
eval' (Mult x y) c = eval' x (MULT y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (ADD y : c) n = eval' y (PLUS n : c)
exec (PLUS n : c) m = exec c (n+m)
exec (MULT y : c) n = eval' y (TIMES n : c)
exec (TIMES n : c) m = exec c (n*m)

value :: Expr -> Int
value e = eval' e []
