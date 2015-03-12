--- Generate and Test  ----
-- n is number of variables, xs is the domain of all variables, cond is the constraint
import Control.Monad
list = [1,2,3,4]

-- example constraint
-- (\[a,b] -> (a<b) && (b/= 3))
-- snd $ head $ filter (\(x,y)-> x == True) 
gentest n xs cond = snd $ head $ filter (\(x,y)-> x == True) [ ((cond a),a) | a <- (replicateM n xs)]