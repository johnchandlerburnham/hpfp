module ChapterExercises where

newtype State s a = State { runState :: s -> (a, s)}

instance Functor (State s) where
  fmap f (State g) =  State $ \s0 -> let (a, s1) = (g s0) in (f a, s1)

instance Applicative (State s) where
  pure a = State (\s0 -> (a, s0))
  (<*>) (State f) (State g) =
    State $ \s0 -> let (a, s1) = (g s0); (f', s2) = (f s1) in (f' a, s2)

instance Monad (State s) where
  return = pure
  (>>=) (State f) g =
    State $ \s0 -> let (a, s1) = (f s0) in (runState (g a)) s1

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \s' -> ((), s)

exec :: State s a -> s -> s
exec (State sa) s0 = let (a, s1) = (sa s0) in s1

eval :: State s a -> s -> a
eval (State sa) s0 = let (a, s1) = sa s0 in a

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), (f s))

