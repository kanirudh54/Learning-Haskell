import Control.Monad

plus2 x = (x+2) --Pure Function

plus2list xs = do (liftM plus2 xs)

--liftM takes pure function and appies it on monad to give a monad.
--Here if plus2 xs is applied we get an error. So we need liftM to do that
