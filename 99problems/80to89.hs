import Control.Monad
data Graph a = Graph [a] [(a, a)] deriving (Show, Eq)

data Adjacency a = Adj [(a, [a])] deriving (Show, Eq)

data Friendly a = Edge [(a, a)] deriving (Show, Eq)

graphToAdj :: (Eq a) => Graph a -> Adjacency a
graphToAdj (Graph [] _)	     = Adj []
graphToAdj (Graph (x:xs) ys) = Adj ((x, ys >>= f) : zs)
   where
      f (a, b)
         | a == x = [b]
         | b == x = [a]
         | otherwise = []
      Adj zs = graphToAdj (Graph xs ys)
