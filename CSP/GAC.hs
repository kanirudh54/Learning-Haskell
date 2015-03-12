GAC::[a]->[[b]]->[String] -- list of variables, list of domains for each variable, list of constraints
GAC v d c =
	let tda = [(x,y)| x <-a, y<-c, x `elem` c ]
		dom = Alg v d c tda
	in dom	


-- Algo for GAC

Alg _ _ _ [] = return []
Alg _ _ _ li@x:xs = 
	let ntda = if null ntda then x:[] else x:ntda
		domvar = check x --check is a function depending on the contraints like i it is binary type or some other format
		bool = if domvar != domain x then do
											nl = xs `union` [(Z,c')|(snd x) `elem` c', x!=c, Z `elem` (snd ntda) ] --gives the domain of x
											domain x = domvar
									  else Alg _ _ _ nl		
