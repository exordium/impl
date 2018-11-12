module Monad.Impl (module X) where
import Impl as X

instance Impl Monad where
  type Methods Monad = '["return","bind"]
  impl m (Arg return) (Arg bind) = [d|
    instance Monad $m where
      return = $return
      (>>=) = $bind
    instance Applicative $m where 
      pure = $return
      mf <*> ma = $bind mf $ \f -> $bind ma $ \a -> $return (f a)
    instance Functor $m where
      fmap f ma = $bind ma $ \a -> $return (f a)
   |]
