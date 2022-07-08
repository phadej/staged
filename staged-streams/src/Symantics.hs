{-# LANGUAGE PolyKinds, TemplateHaskell, TypeFamilies, ScopedTypeVariables, TypeOperators, RankNTypes #-}
module Symantics where

import Language.Haskell.TH.Syntax (Code, Quote)
import Data.Kind (Type)
import Data.SOP (I (..), unI)
import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:))

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

-------------------------------------------------------------------------------
-- Fun
-------------------------------------------------------------------------------

class TermFun (term :: k -> Type) where
    type TyFun term (a :: k) (b :: k) :: k

    termLam :: (term a -> term b) -> term (TyFun term a b)
    termApp :: term (TyFun term a b) -> term a -> term b
