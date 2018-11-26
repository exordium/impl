{-# language TemplateHaskell #-}
module Test where
import Impl
import Named
import Language.Haskell.TH.Syntax
import Language.Haskell.TH
import Language.Haskell.TH.Instances
data Foo a = Foo a

class Bar a where
  bar :: a
  baz :: ()
  baz = ()
return []
ClassI (ClassD _ _ _ _ foo) _ = $(lift =<< reify ''Bar)

{-impl @Monad [t|Foo|]-}
  {-! #return [|Foo|]-}
  {-! #bind [|\(Foo a) f -> f a|]-}
