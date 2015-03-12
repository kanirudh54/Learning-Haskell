data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)
--61
countLeaves Empty                  = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r)         = countLeaves l + countLeaves r

--61A

leaves xs =[helper y | y <- (leav xs)]
helper (Branch x _ _) = x
leav Empty = []
leav t@(Branch _ Empty Empty) = [t]
leav t@(Branch _ l r) = (leav l) ++ (leav r)

--62
inter Empty = []
inter t@(Branch x Empty Empty) = []
inter t@(Branch x l r) = x:(inter l ++ inter r)

--62B

atLevel Empty x = []
atLevel (Branch x l r) 1 = [x]
atLevel (Branch x l r) y = (atLevel l (y-1)) ++ (atLevel r (y-1))

--63

--64
