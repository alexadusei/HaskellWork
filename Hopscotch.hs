module Hopscotch where

-- Main function. Takes a list of numbers and returns a tuple with the best list
-- with the highest score, and that score
hopscotch :: [Int] -> ([Int], Int)
hopscotch list = ((bestPerm list), sum (bestPerm list))

{- 	Auxiliary function to help 'hopscotch'. Recursively breaks the list into
	smaller ones, which two jumps or three. Checks if the sum of the recursive
	call with two jumps is greater than the one with three jumps, if so, it will
	continue down a two-jump recursive call, otherwise it will continue down
	a three-jump call, and returns that list (which is the best list with
	the highest score
-}
bestPerm :: [Int] -> [Int]
bestPerm [] = []
bestPerm (x:xs)
	| length (x:xs) <= 2 = [x]
	| sum (bestPerm (tail (xs))) > sum (bestPerm (tail (tail (xs)))) = 
													x:(bestPerm (tail(xs))) 
	| otherwise = x:(bestPerm (tail(tail(xs))))