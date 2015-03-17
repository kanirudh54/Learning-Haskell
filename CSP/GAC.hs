--Author : Sai Anirudh Kondaveeti
--Some of the code is similar to csp.hs as the logic is same
import Data.List

ld = [('a',[1,2,3,4]),('b',[1,2,3,4]),('c',[1,2,3,4])]
constr = [('a','<','b'),('b','<','c')]

gac::[(Char,[Int])] -> [(Char,Char,Char)] -> [(Char,[Int])] -- list of domains for each variable, list of constraints gives a list of modified domains
gac domain constraints =
	let tda = [(label,constraint)| constraint <- constraints, label  <- [first constraint] ++ [third constraint]]
	in helper tda domain []


helper [] ldomain ntda = ldomain
helper ((label,constraint):xs) ldomain ntda =
	let temp = (label,constraint):ntda
	    olddomain = findl label ldomain                    -- Domain of label which is going to be modified
	    domainother = if (label == first constraint) then (findl (third constraint) ldomain) else (findl (first constraint) ldomain) --domain of other variable in the constraint
	    newdomain = [ y | y <- olddomain, check (label,constraint) domainother y ]
	    newlistdomain = (ldomain \\ [(label,olddomain)]) ++ [(label,newdomain)]
	in (if (olddomain \\ newdomain) /= [] then helper (update (nub temp) xs (label,constraint)) newlistdomain (nub temp) else helper xs newlistdomain (nub temp)) 

--This function adds elemnts from ntda to tda, which needs agian to be checked as the domian of other variable is changed in the constraint
update [] xs (label,constraint) = xs
update ntda xs (label,constraint) =
	let z = [ (l,c) | (l,c) <- ntda, modl (label,constraint) (l,c) ]
	in z ++ xs


modl (label,constraint) (l,c)
	| label == l  = False
	| constraint == c = False
	| ((label == first c) || (label == third c)) = True
	| otherwise = False

--returns the specific domain form the list of domains for given label
findl label ((lab,dom):xs)
	| label == lab = dom
	| otherwise = findl label xs

--checks the constraint and gives the domain of that variable satisfying the constraint
check (label,constraint) domainother val
	| label == first constraint = any (==True) [ verify (val,second constraint,y) | y <- domainother ]
	| otherwise = any (==True) [ verify (y,second constraint,val) | y <- domainother ]


--verifies the (value,binaryfunction,value)
verify::(Int,Char,Int) -> Bool
verify (x,y,z) = case y of '/' -> (x /= z)
                           '<' -> (x < z)
                           '>' -> (x > z)
                           '=' -> (x == z)

--Access Methods of 3-tuple
first::(Char,Char,Char) -> Char
first (x,_,_) = x
second::(Char,Char,Char) -> Char
second (_,x,_) = x
third::(Char,Char,Char) -> Char
third (_,_,x) = x

answer = gac ld constr