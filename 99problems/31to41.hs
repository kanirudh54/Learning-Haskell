import qualified Data.List as List
--31
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime l =
	let ys = [z | z <- [2,3..(l-1)]]
	    zs = map (\x -> l `mod` x) ys
	in 	not(List.any (==0) zs)

--32
gcdt x y =
	let (a,b) = if x>y then (x,y) else if x<y then (y,x) else (x,y)
	    c = (a-b)
	in if a==b then a else gcdt c b

--33
coprime x y = not((gcdt x y) /= 1)

--34
totientphi 1 = 1
totientphi x =
	let xs = [1..(x-1)]
	    ys = map (coprime x) xs
	    zs = map (\z -> if z==True then 1 else 0) ys
	in foldl (\acc x -> acc + x) 0 zs

--35
primeFactors n = primeFactors' n 2
	where primeFactors' 1 _ = []
	      primeFactors' n f
		    | n `mod` f == 0 = f:primeFactors' (n `div` f) f
		    |otherwise       = primeFactors' n (f+1)

--36
prime_factors_mult x =  [(length y, head y) | y <-(List.group $ List.sort $ primeFactors x) ]

--37
phiimproved x =
	let  zs = prime_factors_mult x
	     y = foldl (\acc (x,y) -> (y-1)*(y^(x-1))*acc ) 1 zs
	in y

--38
--No coding involved

--39
primesR low high = [y | y<-[low..high], isPrime y]

--40 -- only to give positive even number
goldbach x =
	let y = primesR 2 x
	    z = primesR 2 x
	    xs = [(p,q)| p <-y, q<-z]
	    ys = filter (\(t,r) -> (t+r) == x) xs
	    d = if (null ys) then (0,0) else (head ys)
	in d

--41
goldbachlist x y =
	let xs = [z | z <- [x..y], even z]
	in map goldbach xs