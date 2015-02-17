data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
--54
--not a question

--55
cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n =
	let (q, r) = (n - 1) `quotRem` 2
	in [Branch 'x' left right | i <- [q .. (q+r)],left <- cbalTree i,right <- cbalTree (n-i-1)]

--56
mirror Empty Empty = True
mirror (Branch _ a b) (Branch _ x y) = mirror a y && mirror b x
mirror _ _ = False

symmetric Empty = True
symmetric (Branch _ l r) = mirror l r

--57

add x Empty = Branch x Empty Empty
add x t@(Branch z l r ) = case compare x z of
                            LT -> Branch z (add x l) r
                            GT -> Branch z l (add x r)
                            EQ -> t

constrct xs = foldl (\acc x -> add x acc) Empty xs

--58
symCbalTrees x = filter symmetric (cbalTree x)

--59
hbalTree x = concatMap cbalTree [2^(x-1)..(2^(x)-1)]


--60
hbalTreenodes = cbalTree --I don't know whether I understood this question correctly h-bal vs completely balanced. 
