-- revPartialList: Returns reversed partial list given bounds
-- the bounds are zero indexed.
-- parameters: xs: the list
--             b1: the lower bound
--             b2: the upper bound
-- Complexity: O(list's length).

revPartialList :: [a] -> Int -> Int -> [a]

revPartialList xs b1 b2 
    | b1 > b2   = []
    | otherwise = take (b2-b1) (reverse (take (b2) xs))

-- largestSss: Returns the largest subsequence sum of a list.
-- Complexity: O(n^2). All the possible lower and upper bounds.

largestSss :: (Num a, Ord a) => [a] -> a

largestSss xs = maximum partialSums
    where partialSums = [sum xs | xs <- rpl]
          rpl = [(revPartialList xs b1 b2) | b1 <- [0..len], b2 <- [b1..len]]
          len = length xs
