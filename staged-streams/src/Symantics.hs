{-# LANGUAGE PolyKinds, InstanceSigs, TemplateHaskell, TypeFamilies, ScopedTypeVariables, TypeOperators, RankNTypes #-}
module Symantics where

import Language.Haskell.TH.Syntax (Code, Quote)
import Data.Kind (Type)
import Data.SOP (I (..), unI)
import Data.Proxy (Proxy (..))
import Data.Coerce (coerce)
import Data.GADT.Compare (GCompare)
import Control.Monad.Fix (MonadFix)
import Language.Haskell.TTH.LetRec (letrecE)

-------------------------------------------------------------------------------
-- let
-------------------------------------------------------------------------------

class SymLet (expr :: k -> Type) where
    termLet
        :: expr a
        -> (expr a -> expr b)
        -> expr b

instance SymLet I where
    termLet t body = coerce body t

instance Quote q => SymLet (Code q) where
    termLet t body = [|| let _letb = $$t in $$(body [|| _letb ||]) ||]

-------------------------------------------------------------------------------
-- letrec
-------------------------------------------------------------------------------

class SymLetRec (expr :: k -> Type) where
    termLetRecH
        :: forall (tag :: k -> Type) (a :: k). GCompare tag
        => (forall m (b :: k). Monad m => (forall c. tag c -> m (expr c)) -> (tag b -> m (expr b))) -- ^ open recursion callback
        -> tag a     -- ^ equation tag
        -> expr a    -- ^ resulting code

instance SymLetRec I where
    termLetRecH
        :: forall tag a. GCompare tag
        => (forall m b. Monad m => (forall c. tag c -> m (I c)) -> tag b -> m (I b))
        -> tag a
        -> I a
    termLetRecH f start = unI (go start)
      where
        -- TODO: this doesn't memoize.
        go :: forall d. tag d -> I (I d)
        go = f go
        

instance (Quote q, MonadFix q) => SymLetRec (Code q) where
    termLetRecH bindf tag0 = letrecE (const "_letrec") bindf (\recf -> recf tag0)

-------------------------------------------------------------------------------
-- Bool
-------------------------------------------------------------------------------

class SymBool (expr :: k -> Type) where
    type TyBool expr :: k

    termIfThenElse :: expr (TyBool expr) -> expr r -> expr r -> expr r

instance SymBool I where
    type TyBool I = Bool

    termIfThenElse cond x y = if unI cond then x else y

instance Quote q => SymBool (Code q) where
    type TyBool (Code q) = Bool

    termIfThenElse cond x y = [|| if $$cond then $$x else $$y ||]

-------------------------------------------------------------------------------
-- List
-------------------------------------------------------------------------------

class SymList (expr :: k -> Type) where
    type TyList expr (a :: k) :: k

    termNil  :: Proxy a -> expr (TyList expr a)
    termCons :: expr a -> expr (TyList expr a) -> expr (TyList expr a)

    termCaseList
        :: expr (TyList expr a)
        -> expr r
        -> (expr a -> expr (TyList expr a) -> expr r)
        -> expr r
        
instance SymList I where
    type TyList I a = [a]

    termNil _ = I []
    termCons (I x) (I xs) = I (x:xs)

    termCaseList (I [])     n _ = n
    termCaseList (I (x:xs)) _ c = c (I x) (I xs)

instance Quote q => SymList (Code q) where
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

class SymFun (expr :: k -> Type) where
    type Arr_ expr (a :: k) (b :: k) :: k

    lam_ :: (expr a -> expr b) -> expr (Arr_ expr a b)
    app_ :: expr (Arr_ expr a b) -> expr a -> expr b

instance SymFun I where
    type Arr_ I a b = a -> b

    lam_ = coerce
    app_ f x = coerce f x

instance Quote q => SymFun (Code q) where
    type Arr_ (Code q) a b = a -> b

    lam_ f = [|| \arg -> $$(f [|| arg ||]) ||]
    app_ f x = [|| $$f $$x ||]
