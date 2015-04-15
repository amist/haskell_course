-- Returns a gray code for n bits
-- Complexity: O(n * 2^n). The nubmer of bits created.

gray :: Integral a => a -> [[Char]]

gray 1 = ["0", "1"]
gray n = first_part ++ second_part
    where first_part = map ('0':) (gray (n-1))
          second_part = map ('1':) (reverse (gray (n-1)))
