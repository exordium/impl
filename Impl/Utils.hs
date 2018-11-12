module Impl.Utils where
import Language.Haskell.TH

typeList :: [Type] -> Type
typeList = foldr (\t -> AppT (AppT PromotedConsT t)) PromotedNilT

-- | Converts a variable number of arguments into curried form. Ex:
--
--  >>> :!kind '[Int,String,Double] :-> IO ()
--  Int -> String -> Double -> IO ()
infixr 0 :->
type family as :-> r where
  (a ': as) :-> r = a -> as :-> r
  '[] :-> r = r

methodsFor :: Name -- ^ Typeclass name
           -> TypeQ -- ^ @ :: [Symbol]@
 
-- | Retrieve the method names of a typeclass as a typelevel list of @Symbol@s.
-- A good default for instantiating the 'Methods' type,
-- which is robust against changing method names
methodsFor n = do
  ClassI (ClassD _ _ _ _ (map sigSym -> ss)) _ <- reify n
  return (typeList ss )
  where sigSym (SigD (nameBase -> s) _) = LitT (StrTyLit s)

