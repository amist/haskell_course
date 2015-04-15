-- cat: concatenate a number (n) at the end of a list (xs)
-- Complexity: O(xs's length).

cat :: a -> [a] -> [a]

cat n xs = xs ++ [n]

-- choose: Returns all the combinations of choosing k elements from n elements.
-- Complexity: O(n choose k). Every element is created with one edge condition.

choose :: Integral a => a -> a -> [[a]]

choose n k
    | k < 1 || k > n = []
    | k == 1         = [[x] | x <- [1..n]]
    | k == n         = perms_with_n
    | otherwise      = perms_with_n ++ perms_without_n
    where perms_with_n = map (cat n) (choose (n-1) (k-1))
          perms_without_n = choose (n-1) k

