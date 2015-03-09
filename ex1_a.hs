-- Which of the numbers in [1, 150] have a single way of representing as a^2+b^2+c^2+d^2
-- The resulted list is the beginning of sequence A006431 in OEIS: https://oeis.org/A006431

-- for elemIndex
import Data.List

-- for fromJust
import Data.Maybe

nums = [ a^2+b^2+c^2+d^2 | a <- [0..13], b <- [a..13], c <- [b..13], d <- [c..13], a^2+b^2+c^2+d^2 <= 150 ]
revNums = reverse nums

-- elemIndex returns the index of the first occurrence, so only unique values will have
-- the same element returned for the regular list and the reversed one
uniqueNums = [ x | x <- [1..150], fromJust(elemIndex x nums) == (length nums) - fromJust(elemIndex x revNums) - 1 ]

main = print uniqueNums
