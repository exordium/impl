{-# language UndecidableInstances #-} -- Only needed for ++ idiom
module Monad.Impl (module X) where
import Data.Maybe
import Impl as X
import Control.Monad.Fail
import GHC.Err

instance Impl Functor where
  type Methods Functor = [Required "fmap", Optional "constMap"]
  impl f (arg #fmap -> fmap) (Arg' constMap') = case constMap' of
    Nothing -> [d|instance Functor $f where fmap = $fmap|]
    Just constMap -> [d|instance Functor $f where fmap = $fmap;(<$) = $constMap|]

instance Impl Applicative where
  type Methods Applicative = '[Required "pure", Required "ap"] ++ Methods Functor
  impl f (arg #pure -> pure) (arg #ap -> ap) fmap constMap
    = concat <$> sequence
    [impl @Functor f fmap constMap
    ,[d|instance Applicative $f where
          pure = $pure
          (<*>) = $ap |]
    ]

instance Impl Monad where
  type Methods Monad = '[Required "return",Required "bind", Optional "fail", Optional "fmap", Optional "constMap"]
  impl m (arg  #return -> return)
         (arg  #bind -> bind)
         (arg' #fail [|errorWithoutStackTrace|] -> fail)
         (Arg' fmap')
         constMap'
    = concat <$> sequence
    [impl @Applicative m
            $$ #pure return
            $$ #ap [|\mf ma -> $bind mf $ \f -> $bind ma $ \a -> $return (f a) |]
            $$ #fmap (fromMaybe [|\f ma -> $bind ma $ \a -> $return (f a)|] fmap')
            $  constMap'
    ,[d|instance Monad $m where
          return = $return
          (>>=) = $bind
          fail = $fail |]
    ]
