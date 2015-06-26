module StateFun where



import Control.Monad
import Data.Monoid




newtype State s a = State { runState :: s -> (a, s) }

instance Show (State s a) where
  show state = "State"

fromStoAandS :: Int -> (String, Int)
fromStoAandS c | c `mod` 5 == 0 = ("foo", c + 1)
               | otherwise = ("bar", c + 1)

stateIntString :: State Int String
stateIntString = State fromStoAandS

instance Monad (State s) where
  return a = State (\s -> (a, s))

  m >>= k = State (\s -> let (a, s') = runState m s
                             in runState (k a) s')

foo :: State Integer String
foo = return "hi"

f :: Integer -> (String,Integer)
f 0 = ("bar", 5)
f _ = ("baz", 6)

ff = State f

bar = (\_ -> ff) =<< foo
