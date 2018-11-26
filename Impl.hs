{-# OPTIONS_HADDOCK show-extensions,not-home #-}
-- | Impl is intended to be used as an alternative to the normal default typeclass methods machinery of Haskell.
-- In contrast with @intrinsic-superclasses@, we must specify each link of the implementation heirarchy with an instance of Impl, rather than infer it from the superclass heirarchy.
-- The benefit of this more explicit style is complete control over default methods provided by subclasses, at the cost of some automation for the class creator.
-- Impl is most valuable when instantiating deep (or even undecidably recursive) typeclass hierarchies for multiple new datatypes, which is most common in client code.
module Impl (
  -- * The core Impl class
  Impl(..),Method(..)
  ,Symbol -- | Reexported from @base@
  ,NamedMethods, NamedExpQ
  ,type (:->)
  -- * Utilities for Named arguments
  -- | @impl@ uses "Named" arguments, which work best with @OverloadedLabels@
  ,type (:!) -- | A required named argument.
             --
             -- >>> #foo 'a' :: "foo" :! Char
  ,type (:?) -- | An optional named argument
             --
             -- >>> #foo 'b' :: "foo" :? Char
  ,($$)
  ,defaults -- | A special 'Param' to fill in the remaining 'Optional' arguments with 'Nothing'
            --
            -- @foo :: ("bar" :! String) -> ("baz" :? Char) -> ("quox" :? Int) -> IO ()@
            --
            -- >>> foo $$ #bar "Hello" $$ defaults :: IO ()
  ,arg {- |

'arg' unwraps a named parameter with the specified name. One way to use it is
to match on arguments with @-XViewPatterns@:

@
fn (arg \#t -> t) (arg \#f -> f) = ...
@

This way, the names of parameters can be inferred from the patterns: no type
signature for @fn@ is required. In case a type signature for @fn@ is
provided, the parameters must come in the same order:

@
fn :: "t" :! Integer -> "f" :! Integer -> ...
fn (arg \#t -> t) (arg \#f -> f) = ... -- ok
fn (arg \#f -> f) (arg \#t -> t) = ... -- does not typecheck
@

-}
  , arg'
  ,Param -- | A parameter passable as a named argument. Used implicitly by '($$)' with @OverloadedLabels
  ,NamedF(Arg,Arg') -- | A named argument that could be required or optional depending on the @f@ parameter

  -- * TH reexports and utilities
  ,methodsFor, type (++), TypeQ, DecsQ
  ) where
import Language.Haskell.TH hiding (Name)
import Named
import Named.Internal (Param,NamedF(ArgF))
import GHC.TypeLits (Symbol)
import Impl.Utils

 
-- | Typeclasses implementing Impl can build declaratios for their entire superclass heirarchy
-- from a collection of required or optional named methods, allowing potentially complex logic for defaulting.
-- See the @example@ internal library for how to implement instances of 'Impl'.
class Impl c where
  type Methods c :: [Method Symbol]
  -- | Instantiate the implementing class along with all its superclasses
  -- Ex: 
  --
  -- > impl @Monad [t|[]|]
  -- >   $$ #return [|\x -> [x]|]
  -- >   $$ #bind   [|flip concatMap|]
  impl :: TypeQ -> NamedMethods c :-> DecsQ

-- | >>> :kind! NamedExpQ '[Required "foo", Optional "bar"]
-- = '["foo" :! ExpQ,"bar" :? ExpQ]
type family NamedExpQ ss where
  NamedExpQ '[] = '[]
  NamedExpQ (Required s ': ss) = (s :! ExpQ) ': NamedExpQ ss
  NamedExpQ (Optional s ': ss) = (s :? ExpQ) ': NamedExpQ ss

-- | "Named" TH 'Exp's for the class method implementations
type NamedMethods c = NamedExpQ (Methods c)

arg' :: Name name -> a -> (name :? a) -> a
{- | A variation of 'arg' for optional arguments. Requires a default value to handle
the case when the optional argument was omitted:

@ fn (arg' \#answer 42 -> ans) = ...  @

In case you want to get a value wrapped in 'Maybe' instead, 'Arg'' -}
arg' = argDef

pattern Arg' :: Maybe a -> name :? a
-- | Construct or match an optional named argument
pattern Arg' a' = ArgF a'

($$) :: WithParam p fn fn' => fn -> Param p -> fn'
{- | Pass a named (optional or required) argument to a function in any order.

     @foo :: ("bar" :! String) -> ("baz" :? Char) -> IO ()@

     >>> foo $$ #baz 'a' :: ("bar" :! String) -> IO ()
-}
($$) = (!)
