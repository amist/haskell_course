-- Splits one list to two lists in the kth place
-- Complexity: O(list's length).

split :: [a] -> Int -> [[a]]

split xs k = [(take k xs), (reverse (take m (reverse xs)))]
    where m = (length xs) - k
