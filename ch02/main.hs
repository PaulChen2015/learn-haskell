double x = x * x
quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns


n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

myLast ns = ns !! (length ns - 1)
myLast2 ns = head (reverse ns)

myInit1 ns = take (length ns - 1) ns
myInit2 ns = reverse (tail (reverse ns))