module Impl.Utils where
import Language.Haskell.TH
import qualified Data.Map as Map

-- | Type level list append
type family (as :: [x]) ++ (bs :: [x]) :: [x] where
  '[] ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)

-- | Converts a variable number of arguments into curried form. Ex:
--
--  >>> :!kind '[Int,String,Double] :-> IO ()
--  Int -> String -> Double -> IO ()
infixr 0 :->
type family as :-> r where
  (a ': as) :-> r = a -> as :-> r
  '[] :-> r = r

data Method a = Required a | Optional a

methodsFor :: Name -- ^ Typeclass name
           -> TypeQ -- ^ @ :: [`Method` `Symbol`]@

{- | Retrieve the method names of a typeclass as a typelevel list of @Method Symbol@s.
     A good default for instantiating the 'Methods' type,
     which is robust against changing method names. -}
{-# DEPRECATED methodsFor "`reify` doesn't currently allow introspecting default definitions,\
   \ so they are always `Required`" #-}
methodsFor n = do
  ClassI (ClassD _ _ _ _ decs) _ <- reify n
  let methods = Map.elems $ foldr addDecl Map.empty decs
  return (typeList methods)
    where
      mkOptional = \case Required a -> Optional a; Optional a -> Optional a
      addDecl = \case
                SigD n _ty -> Map.insert n
                   $ Required (LitT (StrTyLit (nameBase n)))
                ValD (VarP n) _ _ -> Map.adjust mkOptional n
                FunD n _          -> Map.adjust mkOptional n
      typeList :: [Method Type] -> Type
      typeList = foldr (cons . methodType)  PromotedNilT where
        cons t = AppT (AppT PromotedConsT t)
        methodType = \case
          Required t -> ConT 'Required `AppT` t
          Optional t -> ConT 'Optional `AppT` t
