import qualified Control.Monad as M
import qualified System.IO as Io
import Data.Ord (comparing)
import qualified Data.List as List
--46
not' True = False
not' False = True

and' True True = True
and' _    _    = False

or' False False = False
or' _     _     = True

nor' a b = not' $ or' a b

nand' a b = not' $ and' a b

xor' True False = True
xor' False True = True
xor' _     _    = False

impl' a b = (not' a) `or'` b

equ' True True   = True
equ' False False = True
equ' _     _     = False

table f = concatMap (++ "\n") [printline f x y | x <-[True,False], y <- [True,False] ]

printline f x y = show x ++ " " ++ show y ++ " " ++ show (f x y)

--47
--Directly works as it has same 

--48
tablen n f = M.mapM_ putStrLn [toStr a ++ " => " ++ show (f a) | a <- args n]
    where args n = M.replicateM n [True, False]
          toStr = unwords . map (\x -> show x ++ space x)
          space True = "  "
          space False = " "

--49
gray 0 = [""]
gray n =
	let xs = gray (n-1)
	in map ('0':) xs ++ map ('1':) (reverse xs)

--50
data HTree a = Leaf a | Branch (HTree a) (HTree a) deriving Show

huffman freq = List.sortBy (comparing fst) $ serialize $
        htree $ List.sortBy (comparing fst) $ [(w, Leaf x) | (x,w) <- freq]
  where htree [(_, t)] = t
        htree ((w1,t1):(w2,t2):wts) =
                htree $ List.insertBy (comparing fst) (w1 + w2, Branch t1 t2) wts
        serialize (Branch l r) =
                [(x, '0':code) | (x, code) <- serialize l] ++
                [(x, '1':code) | (x, code) <- serialize r]
        serialize (Leaf x) = [(x, "")]