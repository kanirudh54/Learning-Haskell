-- ns is the list of nodes it is represent as (label, value). initial value is (char,'u') 
-- es is the list of edges --each tuple is an edge
-- ld is the list of domains for each node -- (label,domain)
ns = [('a','u'),('b','u'),('c','u'),('d','u'),('e','u'),('f','u')]
es = [('a','b'),('a','c'),('b','c'),('b','d'),('c','d'),('c','e'),('d','e'),('e','f'),('c','f'),]
ld = [('a',[1,2,3]),('b',[1,2,3]),('c',[1,2,3]),('d',[1,2,3]),('e',[1,2,3]),('f',[1,2,3])]
-- cond is the condition
-- ((a/=b)&&(a/=c)&&(b/=c)&&(b/=d)&&(c/=d)&&(c/=e)&&(d/=e)&&(c/=f)&&(e/=f))
ForwardCheck (ns,es) ld cond = ForwardCheck' (ns,es) sld cond
	where sld = sorth ld
--mods modified list of domains
ForwardCheck' (ns,es) [(x,[])] cond = []
ForwardCheck' (ns,es) (x:xs) cond = [(fst x, y):((ForwardCheck (ns,es) mods cond)++(ForwardCheck (ns,es) mods cond))| y <- (snd x) ,let mods = helper (fst x) y xs cond]

helper x y xs cond = [z | z <- xs,  (x==y)&&cond]  ---- How to modify the domain ??




--MRV--
sorth [] = []
sorth (x:xs) =
	let ys = sorth [y | y <- xs, (length (snd y)) < (length (snd x))]
	    zs = sorth [y | y <- xs, (length (snd y)) >= (length (snd x))]
	in ys ++ [x] ++ zs