{-# LANGUAGE PolyKinds, InstanceSigs, TemplateHaskell, TypeFamilies, ScopedTypeVariables, TypeOperators, RankNTypes #-}
module Symantics where

import Language.Haskell.TH.Syntax (Code, Quote)
import Data.Kind (Type)
import Data.SOP (I (..), unI)
import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:))
import Data.Coerce (coerce)

import Staged.Commons (sletrecH, MonadFix_)

-------------------------------------------------------------------------------
-- letrec
-------------------------------------------------------------------------------

class TermLetRec (term :: k -> Type) where
    termLetRecH
        :: forall (tag :: k -> Type) (a :: k).
           (forall x y. tag x -> tag y -> Maybe (x :~: y)) -- ^ equality on equation tags
        -> (forall m (b :: k). Monad m => (forall c. tag c -> m (term c)) -> (tag b -> m (term b))) -- ^ open recursion callback
        -> tag a     -- ^ equation tag
        -> term a    -- ^ resulting code

instance TermLetRec I where
    termLetRecH
        :: forall tag a. (forall x y. tag x -> tag y -> Maybe (x :~: y))
        -> (forall m b. Monad m => (forall c. tag c -> m (I c)) -> tag b -> m (I b))
        -> tag a
        -> I a
    termLetRecH _eq f start = unI (go start)
      where
        -- TODO: this doesn't memoize.
        go :: forall d. tag d -> I (I d)
        go = f go
        

instance (Quote q, MonadFix_ q) => TermLetRec (Code q) where
    termLetRecH = sletrecH


-------------------------------------------------------------------------------
-- Bool
-------------------------------------------------------------------------------

class TermBool (term :: k -> Type) where
    type TyBool term :: k

    termIfThenElse :: term (TyBool term) -> term r -> term r -> term r

instance TermBool I where
    type TyBool I = Bool

    termIfThenElse cond x y = I (if unI cond then unI x else unI y)

instance Quote q => TermBool (Code q) where
    type TyBool (Code q) = Bool

    termIfThenElse cond x y = [|| if $$cond then $$x else $$y ||]

-------------------------------------------------------------------------------
-- List
-------------------------------------------------------------------------------

class TermList (term :: k -> Type) where
    type TyList term (a :: k) :: k

    termNil  :: Proxy a -> term (TyList term a)
    termCons :: term a -> term (TyList term a) -> term (TyList term a)

    termCaseList
        :: term (TyList term a)
        -> term r
        -> (term a -> term (TyList term a) -> term r)
        -> term r
        
instance TermList I where
    type TyList I a = [a]

    termNil _ = I []
    termCons (I x) (I xs) = I (x:xs)

    termCaseList (I [])     n _ = n
    termCaseList (I (x:xs)) _ c = c (I x) (I xs)

instance Quote q => TermList (Code q) where
    type TyList (Code q) a = [a]

    termNil _ = [|| [] ||] 
    termCons x xs = [|| $$x : $$xs ||]

    termCaseList xs n c = [|| case $$xs of
        []   -> $$n
        y:ys -> $$(c [|| y ||] [|| ys ||])
        ||]

-------------------------------------------------------------------------------
-- Fun
-------------------------------------------------------------------------------

class TermFun (term :: k -> Type) where
    type TyFun term (a :: k) (b :: k) :: k

    termLam :: (term a -> term b) -> term (TyFun term a b)
    termApp :: term (TyFun term a b) -> term a -> term b

instance TermFun I where
    type TyFun I a b = a -> b

    termLam = coerce
    termApp (I f) (I x) = I (f x)

instance Quote q => TermFun (Code q) where
    type TyFun (Code q) a b = a -> b

    termLam f = [|| \x -> $$(f [|| x ||]) ||]
    termApp f x = [|| $$f $$x ||]
