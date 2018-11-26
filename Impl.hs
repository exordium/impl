{-# OPTIONS_HADDOCK show-extensions #-}
-- | This small but extensible framework facilitates defining complex defaulting rules that are not handled by @DefaultSignatures@, and reducing the overhead of giving instances to new datatypes by generating superclasses. One reason we might want this is when a superclass wants to be given a default by two different subclasses (ex: @Bifunctor@ and @Profunctor@ both could generate @Functor@ instances). See the @example@ internal library for how to implement instances of 'Impl'.
module Impl (Impl(..),Method(..),Symbol, NamedMethods
  -- * Reexports
  ,type (:->),TypeQ,($$), type (:!), type (:?), Name
  ,NamedF(Arg,Arg'), arg, arg', defaults
  ,methodsFor, type (++)
  ) where
import Language.Haskell.TH hiding (Name)
import Named
import Named.Internal (Param,NamedF(ArgF))
import GHC.TypeLits (Symbol)
import Impl.Utils

 
class Impl c where
  type Methods c :: [Method Symbol]
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
  NamedExpQ (Required s ': ss) = (s :! ExpQ) ': NamedExpQ ss
  NamedExpQ (Optional s ': ss) = (s :? ExpQ) ': NamedExpQ ss

-- | "Named" TH 'Exp's for the class method implementations
type NamedMethods c = NamedExpQ (Methods c)

arg' :: Name name -> a -> (name :? a) -> a
arg' = argDef

pattern Arg' :: Maybe a -> name :? a
pattern Arg' a' = ArgF a'

($$) :: WithParam p fn fn' => fn -> Param p -> fn'
($$) = (!)
