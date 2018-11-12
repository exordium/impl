{-# OPTIONS_HADDOCK show-extensions #-}
-- | This small but extensible framework facilitates defining complex defaulting rules that are not handled by @DefaultSignatures@, and reducing the overhead of giving instances to new datatypes by generating superclasses. One reason we might want this is when a superclass wants to be given a default by two different subclasses (ex: @Bifunctor@ and @Profunctor@ both could generate @Functor@ instances). See the @example@ internal library for how to implement instances of 'Impl'.
module Impl (Impl(..),NamedMethods
  -- * Reexports
  ,type (:->),TypeQ,(!) ,type (:!), NamedF(Arg), arg
  ) where
import Language.Haskell.TH
import Named hiding (Name)
import GHC.TypeLits (Symbol)
import Impl.Utils
 
class Impl c where
  type Methods c :: [Symbol]
  -- | Instantiate the implementing class along with all its superclasses
  -- Ex: 
  --
  -- > impl @Monad [t|[]|]
  -- >   ! #return [|\x -> [x]|]
  -- >   ! #bind   [|flip concatMap|]
  impl :: TypeQ -> NamedMethods c :-> DecsQ

-- | 
type family NamedExpQ ss where
  NamedExpQ '[] = '[]
  NamedExpQ (s ': ss) = (s :! ExpQ) ': NamedExpQ ss

-- | "Named" TH 'Exp's for the class method implementations
type NamedMethods c = NamedExpQ (Methods c)
