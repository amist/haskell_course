-- rlc: Run Length Code
-- myRlc: A helper function for returning partial run length code
--     parameters: lastChar: the last char counted
--                 counter: the count for the lastChar
--                 xs: the partial list to be run lengthed
-- Complexity: O(list's length).

myRlc :: Integral a => a -> a -> [a] -> [a]

myRlc 0 _ [] = []
myRlc lastChar counter [] = counter : lastChar : []
myRlc lastChar counter xs
    | x0 == lastChar || counter == 0 = myRlc x0 (counter+1) (tail xs)
    | otherwise                       = counter : lastChar : (myRlc x0 0 xs)
    where x0 = head xs


rlc :: Integral a => [a] -> [a]

rlc lst = myRlc 0 0 lst
