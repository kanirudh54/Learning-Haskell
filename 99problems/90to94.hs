import Data.List
--90
generateSolutions' 0 = [[]]
generateSolutions' n = permutations [1..n]

duplicateDiagonals q qs = any (\(columnDistance, row) -> abs (row - q) == columnDistance) $ zip [1..] qs

test [] = True
test (q:qs) = not (duplicateDiagonals q qs) &&test qs

queens n = filter test (generateSolutions' n) 

--91
li n = [(x,y) | x <- [1..n], y <- [1..n]]
generateSol n = filter (\xs -> (xs!!0) == (1,1)) (permutations (li n))

help x y =
	let (r,c) = x
	    (l,k) = y
	in if (((r+2,c+1) == (l,k)) || ((r+2,c-1) == (l,k)) || ((r+1,c+2) == (l,k)) || ((r+1,c-1) == (l,k)) || ((r-1,c+2) == (l,k)) || ((r-1,c-2) == (l,k)) || ((r-2,c+1) == (l,k)) || ((r-2,c-1) == (l,k))) then True else False

teste (x:y:xs)
	| help x y == True = teste (y:xs)
	| help x y == False = []

knight n = filter (\xs -> xs /= [])(map (\xs -> teste xs) (generateSol n))
