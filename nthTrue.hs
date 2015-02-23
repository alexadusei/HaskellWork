module NthTrue where

-- Program finds the nth true statement of any function, given a function and 
-- a number value (the nthTrue value.)

nthTrue :: (Int -> Bool) -> Int -> Int
nthTrue f n
	| n < 1 = error "Number cannot be less than 1"
	| otherwise = findTrues f n 1
	where
	{- Helper function has three parameters: a function, number of Trues we are
		looking for, and the starting index for use to find Trues (starting at
		1). If the function returns True, we decrease the amount of Trues we 
		are looking for, while increasing the index to test the next number with
		the function parameter. If the number of Trues we are looking for hits 
		0, we return the index, which is the nth True index. -} 
		findTrues :: (Int -> Bool) -> Int -> Int -> Int
		findTrues func numTrues i
			| numTrues == 0 		= i-1
			| func i == False 		= findTrues func numTrues (i+1)
			| otherwise 			= findTrues func (numTrues-1) (i+1)


isPerfect :: Int -> Bool
isPerfect n
	| n < 1 	= False
	| otherwise = (isFactor n 1) == n
	where
	{- 	Auxiliary function uses the same parameter n and a parameter i, which
		represents the counter from 1, checks all factors of n and returns the
		sum -}
		isFactor :: Int -> Int -> Int
		isFactor n i
			| i >= n 		= 0
			| mod n i /= 0 	= isFactor n (i+1)
			| otherwise 	= i + isFactor n (i+1)

isPrime n 
	| n <= 1 	= False
	| otherwise = not (hasFactorInRange n 2 sqrtFloor)
	where
	-- sqrtFloor is the square root of n rounded down to an integer
	-- fromIntegral takes int and changes into integral (float)
	sqrtFloor = floor (sqrt (fromIntegral n))

-- Auxiliary function to isPrime
hasFactorInRange :: Int -> Int -> Int -> Bool
hasFactorInRange n low high
	| low > high 		= False
	| mod n low == 0 	= True
	| otherwise 		= hasFactorInRange n (low+1) high 
