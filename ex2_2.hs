-- gcd' - to distinguish from the standard gcd
-- Returns the gcd of two numbers
-- Complexity: about O(log min(a, b)): Euclid's algorithm.

gcd' :: Integral a => a -> a -> a

gcd' 1 x = 1
gcd' x 1 = 1
gcd' x y
    | x `mod` y == 0 = y
    | y `mod` x == 0 = x
    | x < y  = gcd' x (y `mod` x)
    | x > y  = gcd' (x `mod` y) y

