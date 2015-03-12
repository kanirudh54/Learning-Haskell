import qualified Data.List as List
pack::(Eq a)=>[a]->[[a]]  --9
pack = List.group
encode z = [(length x,head x)| x<- pack z] --10

data ListItem a = Sin a |Mul Int a deriving (Show) --11 
encodeModified xs = [y | x <- List.group xs, let y = if (length x) == 1 then Sin (head x) else Mul (length x) (head x)]

--12
decodeModified xs = [y | z<-xs, let y = helper z where helper (Sin x) = [x]
						       helper (Mul n x) = replicate n x]

--13
encodedirect' []=[]
encodedirect' xs =
	let (li,vi) = span (==head xs) xs
	in (length li, head li):encodedirect' vi

encodeDirect [] = []
encodeDirect xs = map helper z where helper (1,n) = (Sin n)
				     helper (n,x) = (Mul n x)
				     z = encodedirect' xs 
--14
dupli [] = []
dupli xs = concatMap (replicate 2) xs

--15
repli [] x = []
repli xs 0 = []
repli xs x = concat [y | z<-xs, let y = replicate x z]

--16
dropEvery [] x = []
dropEvery xs 0 = xs
dropEvery xs x = [z | z<-xs, fmap (`mod` 3) (z `List.elemIndex` xs) /= Just 2 ]

--17
split::[Char]->Int->[[Char]]
split xs x =
	let ys = [y | y<-xs, (y `List.elemIndex` xs) < Just x]
	    zs = [l | l<-xs, (l `List.elemIndex` xs) > Just (x-1)]
	in ys:zs:[]

--18

split' xs x =
	let ys = [y | y<-xs, (y `List.elemIndex` xs) < Just (x-1)]
	    zs = [l | l<-xs, (l `List.elemIndex` xs) > Just (x-2)]
	in ys:zs:[]
slice xs x y = (split' ((split' xs x)!!1) y)!!0

--19
rotate xs x =
	let ys = [y | y<-xs, (y `List.elemIndex` xs) < Just x]
	    zs = [l | l<-xs, (l `List.elemIndex` xs) > Just (x-1)]
	in concat (zs:ys:[])

--20
removeAt x xs = 
	let z = [y | y<-xs, y`List.elemIndex` xs == Just (x-1)]
	    w = [y | y<-xs, y`List.elemIndex` xs /= Just (x-1)]
	in z:w:[]
