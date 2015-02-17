import qualified Data.List as List

mylast x = last x  --1

mybutlast x = (reverse x)!!1

elementAt x y = x!!(y-1)

mylength x = length x --4

myRev :: [a] -> [a] --5
myRev [] = []
myRev (x:xs) = myRev xs ++ [x]

isPalindrome [] = False --6
isPalindrome li@(x:xs) = reverse li == li

data NestedList a = Elem a | List [NestedList a] --7
 
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x
{-|
flatten [] = []
flatten [x] = [x]
flatten li@(x:xs) = flatten x ++ flatten xs
-}

compress::(Eq a)=>[a]->[a] --8
compress [] = []
compress [x] = [x]
compress (x:y:ys)
 	| x == y    = compress (y:ys)
 	| otherwise = x:compress (y:ys)

pack::(Eq a)=>[a]->[[a]]  --9
pack = List.group


encode z = [(length x,head x)| x<- pack z] --10


