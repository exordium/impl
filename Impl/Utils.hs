module Impl.Utils where
import Language.Haskell.TH
import qualified Data.Map as Map

typeList :: [Method Type] -> Type
typeList = foldr (cons . methodType)  PromotedNilT where
  cons t = AppT (AppT PromotedConsT t)
  methodType = \case
    Required t -> ConT 'Required `AppT` t
    Optional t -> ConT 'Optional `AppT` t

-- | Converts a variable number of arguments into curried form. Ex:
--
--  >>> :!kind '[Int,String,Double] :-> IO ()
--  Int -> String -> Double -> IO ()
infixr 0 :->
type family as :-> r where
  (a ': as) :-> r = a -> as :-> r
  '[] :-> r = r

data Method a = Required a | Optional a
getMethod :: Method a -> a
getMethod = \case Required a -> a; Optional a -> a

methodsFor :: Name -- ^ Typeclass name
           -> TypeQ -- ^ @ :: [Method Symbol]@
 
-- | Retrieve the method names of a typeclass as a typelevel list of @Required Symbol@s.
-- A good default for instantiating the 'Methods' type,
-- which is robust against changing method names
methodsFor n = do
  {-ClassI (ClassD _ _ _ _ (map sigSym -> ss)) _ <- reify n-}
  ClassI (ClassD _ _ _ _ decs) _ <- reify n
  let methods = Map.elems $ foldr foo Map.empty decs
  return (typeList methods)
foo = \case
          SigD n _ty -> Map.insert n
             $ Required (LitT (StrTyLit (nameBase n)))
          ValD (VarP n) _ _ -> Map.adjust (Optional . getMethod) n
          FunD n _          -> Map.adjust (Optional . getMethod) n


type family (as :: [x]) ++ (bs :: [x]) :: [x] where
  '[] ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)
