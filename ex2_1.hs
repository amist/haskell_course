-- Returns the kth element from the end of the list
-- Complexity: O(list's length), because we have to reverse the list.
kthFromEnd :: Int -> [a] -> a

kthFromEnd k xs = last (take k (reverse xs))

