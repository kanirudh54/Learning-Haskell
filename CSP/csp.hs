--author : Sai Anirudh Kondaveeti
import Data.List as L
-- ld is the list of domains for each variable -- (label,domain)

ld = [('a',[1,2,3,4]),('b',[1,2,3,4]),('c',[1,2,3,4]),('d',[1,3,4])]
-- condi is the condition
condi = [('a','/','b'),('b','/','c'),('b','<','c'),('b','<','d'),('a','>','c'),('c','<','d')]
{--!
ForwardCheck (ns,es) ld cond = ForwardCheck' (ns,es) sld cond
	where sld = sorth ld
--mods modified list of domains
ForwardCheck' (ns,es) [(x,[])] cond = []
ForwardCheck' (ns,es) ((var,dom):xs) cond = [(var, y):((ForwardCheck (ns,es) mods cond)++(ForwardCheck (ns,es) mods cond)) | y <- dom ,let mods = helper var y xs cond]

helper x y xs cond = [z | z <- xs,  (x==y) && cond]  ---- How to modify the domain ??
--}

--checks whether the given label and values satisfy the condition
check::(Int,Int)->(Char,Char) -> [(Char,Char,Char)] -> Bool
check (value1,value2) (label1,label2) xs =
	let (x,y,z) = (chec (value1,value2) (label1,label2) xs)!!0
	    bool = if (label1 == x) then (verify (value1,y,value2)) else (verify (value2,y,value1))
	in bool

--verifies the (value,binaryfunction,value)
verify::(Int,Char,Int) -> Bool
verify (x,y,z) = case y of '/' -> (x /= z)
                           '<' -> (x < z)
                           '>' -> (x > z)
                           '=' -> (x == z)

--Gives the exact condition in with those labels are present
chec::(Int,Int) -> (Char,Char) -> [(Char,Char,Char)] -> [(Char,Char,Char)]
chec (value1,value2) (label1,label2) [] = []
chec (value1,value2) (label1,label2) ((x,y,z):xs) =
	let  cond = if ((label1 == x && label2 == z) || (label1 == z && label2 == x)) then [(x,y,z)] else chec (value1,value2) (label1,label2) xs
	in cond

--MRV--
sorth::[(Char, [Int])] -> [(Char, [Int])]
sorth [] = []
sorth (x:xs) =
	let ys = sorth [y | y <- xs, (length (snd y)) < (length (snd x))]
	    zs = sorth [y | y <- xs, (length (snd y)) >= (length (snd x))]
	in (ys ++ [x] ++ zs)

--The Algorithm
search::[(Char,[Int])] -> [(Char,Int)] -> [(Char,Char,Char)] -> Maybe [(Char,Int)]
search [] instantiatedVars _ = Just instantiatedVars
search freeVars instantiatedVars cond =
       if (null domain) then
           Nothing
       else
           findSolution [search (restrict sortedFreeVars cond (label,val)) ((label,val):instantiatedVars) cond | val <- domain]

       where
           ((label,domain):sortedFreeVars) = sorth freeVars	



findSolution [] = Nothing
findSolution (Nothing:xs)  =  findSolution xs
findSolution ((Just x):xs) =  Just x
		     


--Access Methods of 3-tuple
first::(Char,Char,Char) -> Char
first (x,_,_) = x
second::(Char,Char,Char) -> Char
second (_,x,_) = x
third::(Char,Char,Char) -> Char
third (_,_,x) = x

--extractLabels gives the list of labels present , whose domain needs to be checked
extractLabels::[(Char,Char,Char)] -> Char -> [Char]
extractLabels xs label = [ y | (x,op,y)<-xs, x == label] ++ [ x | (x,op,y)<-xs, y == label]

-- this resticts the domain of each varibale
restrict::[(Char,[Int])] -> [(Char,Char,Char)] -> (Char,Int) -> [(Char,[Int])]
restrict [] _ _ = []
restrict xs condition (label,value) = --we need to check the list xs in which the label is there. find give the refined list
	let relevantConditions = [ (x,op,y) | (x,op,y)  <- condition, (label == x || label == y)]
	    labels = extractLabels relevantConditions label
	    freevarsincondition = [ (a,b) | (a,b) <- xs, a `elem` labels ]
	    freevarsnotincondition = xs L.\\ freevarsincondition
	in freevarsnotincondition ++ [ (l,newdomain) | (l,dom) <- freevarsincondition, let newdomain = [y | y <- dom, (check (value,y) (label,l) relevantConditions)] ] --addthe difference

answer = search ld [] condi