-- How many times you can reach a number with the addition of 1, 2, 3 (with order).
-- for example, you can reach the number 4 in 7 ways:
-- 3 + 1
-- 2 + 2
-- 2 + 1 + 1
-- 3 + 1
-- 1 + 2 + 1
-- 1 + 1 + 2
-- 1 + 1 + 1 + 1

-- first way: recursion
decompSum1 n =
    if n >= 3 
        then decompSum1 (n-3) + decompSum1 (n-2) + decompSum1 (n-1)
        else if n == 2
            then decompSum1 (n-2) + decompSum1 (n-1)
            else 1
            

-- second way: zipWith - like Fibonacci but with three elements
decompSumList = 1 : 1 : 2 : zipWith (+) decompSumList (tail (zipWith (+) decompSumList (tail decompSumList)))
            
decompSum2 n = decompSumList !! n
