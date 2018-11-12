{-# language TemplateHaskell #-}
module Test where
import Impl
import Named
data Foo a = Foo a


impl @Monad [t|Foo|]
  ! #return [|Foo|]
  ! #bind [|\(Foo a) f -> f a|]
