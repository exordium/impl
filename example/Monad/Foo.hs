module Monad.Foo where
import Monad.Impl

data Foo a = Foo a

impl @Monad [t|Foo|]
  $$ #return [|Foo|]
  $$ #bind   [|\(Foo a) f -> f a|]
  $$ #fmap   [|\f (Foo a) -> Foo (f a)|]
  $$ defaults
