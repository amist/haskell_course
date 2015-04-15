-- Inserts a new element into the kth place of the list
-- Complexity: O(k).

insertKth :: (Eq a, Integral b) => a -> b -> [a] -> [a]

insertKth x 0 xs = x : xs
insertKth x k xs = (head xs) : (insertKth x (k-1) (tail xs))

