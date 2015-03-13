--Author : Sai Anirudh Kondaveeti 

import Data.List

{--! Excercise 1 --}

data PathLength a = Infinity | Normal a deriving Show

instance (Eq a) => Eq (PathLength a) where
	Infinity == _ = False             --If infinity is there the answer is always False.
	_ == Infinity = False
	(Normal a) == (Normal b) = a == b

plus::(Num a)=>(PathLength a)->(PathLength a)->(PathLength a)
plus Infinity _ = Infinity
plus _ Infinity = Infinity
plus (Normal a) (Normal b) = Normal (a + b)

extractValue::(PathLength a) -> a
extractValue (Normal x) = x

specialMin::(Ord a)=>[PathLength a]->(PathLength a)
specialMin [] = error "Empty List"
specialMin [x] = x
specialMin [x,Infinity] = x 
specialMin (Infinity:xs) = specialMin xs
specialMin ((Normal a):xs) = if (a < extractValue (specialMin xs)) then (Normal a) else specialMin xs

{--! Part c

(Eq a) is a class constraint. It restricts the values 'a' can take i.e type of 'a' has to be part of
'Eq' class. For example integers, float, characters. This is necessary because the third step which is 
(Normal a) == (Normal b) , checks a == b . If I don't give (Eq a) then the expresion 'a==b' becomes
invalid	

--}

{--! Part d

(Num a) is a class constraint.It restricts the values 'a' can take i.e type of 'a' has to be part of
'Num' class. For example 'a' can be Int, Integer, Float, Double. This is necessary because the last
step of the 'plus' function contains 'Normal (a + b)' . For (a + b) to be defined we need (Num a). 
'+' is a method of Num type class

--}

{--! Exercise 2 --}
data Page = Page {uid :: Integer, content :: String, links :: [Integer]} deriving (Show,Eq) -- using record syntax so that accessing would be easier


-- Intilizing the data as given in assignment

page1 = Page {uid = 1, content = "This page 1", links = [23,32,45]}
page23 = Page {uid = 23, content = "This page 23", links = [19]}
page32 = Page {uid = 32, content = "This page 32", links = [7,66]}
page45 = Page {uid = 45, content = "This page 45", links = [66]}
page7 = Page {uid = 7, content = "This page 7", links = [19]}
page66 = Page {uid = 66, content = "This page 66", links = []}
page19 = Page {uid = 19, content = "This page 19", links = []}

--data for replacement of pages

page2 = Page {uid = 1, content = "This page 2", links = []}
page3 = Page {uid = 3, content = "This page 3", links = []}
page4 = Page {uid = 4, content = "This page 4", links = []}


initiallist = [page1,page19,page66,page23,page32,page45,page7]
replacement = [page2,page3,page4]

findPage::Integer->[Page]->(Maybe Page)
findPage index [] = Nothing
findPage index (x:xs) = if (index == (uid x)) then Just x else findPage index xs

--There is some ambiguity in toPage, So I will write out my interpretation. We are given list of ids, a list of pages
--It is told that if we have to replace the page with that id , with correspnding page in the list of pages. So we are 
--assuming here that we already have alist of pages which we want to replace with another list. So during this process
-- It is assumed that the links of old page and new page are different and the links for other pages will be updated
--accordingly. Also another assumption is that we are not replacing the newpages i.e we are only checking for pages
--to be replaced from initial list.
--toPage [] pagelist = pagelist

toPage::[Integer]->[Page]->[Page]
toPage [] [] = []
toPage il pl =
	let nonmodifiedlistofpages = [ y | y <- initiallist, not((uid y) `elem` il)]   --- These will be present in final answer,so seperating them
	    tobemodifiedlistofpages = [ y | y <- initiallist, (uid y) `elem` il] --pages which will be replaced
	    uidoftobemodified  = [uid y | y <- tobemodifiedlistofpages]   -- ids of pages which will be replaces
	    newnonmodifiedlistofpages = [Page (uid y) (content y) (modify (links y) uidoftobemodified) | y <- nonmodifiedlistofpages] -- Removes the links for pages which are to be deleted
	    newpageswhichareadded =  [y | y <- pl, z <- uidoftobemodified,  z `elemIndex` il == y `elemIndex` pl]--Only valid pages will be replaced. Validity depends on initial list of indices 
	in newpageswhichareadded ++ newnonmodifiedlistofpages

--helper function in toPage which modifies the links of pages which are not replaced
modify xs ys = [y | y <-xs, not(y `elem` ys)]



---This is the helper function which gives the shortest path between two pages. The second argument is the pages in the graph
shortpath :: Integer->[Page]->Integer->(PathLength Integer)
shortpath id initiallist start
	| start == id = (Normal 0)   
	| ((start /= id) && ((modlinks (findPage start initiallist)) == [])) = (Infinity)   -- Check if the links is null. If it is null then the distance will be Infinite
	| otherwise = specialMin [(Normal 1) `plus` (shortpath id initiallist newstart) | newstart <- (modlinks (findPage start initiallist))]  -- Calculayes all the distances and took the minimum

--helper function to give just the values. It gives the list of links. This the modified version of 'links' . links only works on 'Page' . Modified links works on 'Maybe Page'
modlinks Nothing = []
modlinks (Just x) = links x


--Actual function which is asked. I couldn't translate properly the question, so I am writing my interpretation. The output is of type PathLength Ineger, So that I considerred the minimum path
--from start to all destinantions. If we want a list of shortestpath for each defination, we just need to remove the 'specialMin' function and change the output type to [PathLength Ineger] in function defination
shortestPath :: [Integer]->[Page]->Integer->(PathLength Integer)
shortestPath destinations initiallist start = specialMin [(shortpath id initiallist start) | id <- destinations]