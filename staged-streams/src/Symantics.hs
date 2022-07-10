{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Symantics where

import Control.Monad.Fix           (MonadFix)
import Data.Coerce                 (coerce)
import Data.GADT.Compare           (GCompare)
import Data.Kind                   (Type)
import Data.Proxy                  (Proxy (..))
import Data.SOP                    (I (..), unI)
import Language.Haskell.TH.Syntax  (Code, Quote)
import Language.Haskell.TTH.LetRec (letrecH)

-------------------------------------------------------------------------------
-- let
-------------------------------------------------------------------------------

class SymLet (expr :: k -> Type) where
    let_ :: expr a -> (expr a -> expr b) -> expr b
    let' :: expr a -> (expr a -> expr b) -> expr b

instance SymLet I where
    let_ t body = coerce body t
    let' t body = coerce body t

instance Quote q => SymLet (Code q) where
    let_ t body = [|| let  _letb = $$t in $$(body [|| _letb ||]) ||]
    let' t body = [|| let !_letb = $$t in $$(body [|| _letb ||]) ||]

-------------------------------------------------------------------------------
-- letrec
-------------------------------------------------------------------------------

class SymLetRec (expr :: k -> Type) where
    letrec_
        :: forall (tag :: k -> Type) (a :: k). GCompare tag
        => (forall m (b :: k). Monad m => (forall c. tag c -> m (expr c)) -> (tag b -> m (expr b))) -- ^ open recursion callback
        -> tag a     -- ^ equation tag
        -> expr a    -- ^ resulting code

instance SymLetRec I where
    letrec_
        :: forall tag a. GCompare tag
        => (forall m b. Monad m => (forall c. tag c -> m (I c)) -> tag b -> m (I b))
        -> tag a
        -> I a
    letrec_ f start = unI (go start)
      where
        -- TODO: this doesn't memoize.
        go :: forall d. tag d -> I (I d)
        go = f go


instance (Quote q, MonadFix q) => SymLetRec (Code q) where
    letrec_ bindf tag0 = letrecH (const "_letrec") bindf (\recf -> recf tag0)

-------------------------------------------------------------------------------
-- Bool
-------------------------------------------------------------------------------

class SymBool (expr :: k -> Type) where
    type Bool_ expr :: k

    ife_ :: expr (Bool_ expr) -> expr r -> expr r -> expr r

instance SymBool I where
    type Bool_ I = Bool

    ife_ cond x y = if unI cond then x else y

instance Quote q => SymBool (Code q) where
    type Bool_ (Code q) = Bool

    ife_ cond x y = [|| if $$cond then $$x else $$y ||]

-------------------------------------------------------------------------------
-- List
-------------------------------------------------------------------------------

class SymList (expr :: k -> Type) where
    type List_ expr (a :: k) :: k

    nil_  :: Proxy a -> expr (List_ expr a)
    cons_ :: expr a -> expr (List_ expr a) -> expr (List_ expr a)

    caseList_
        :: expr (List_ expr a)
        -> expr r
        -> (expr a -> expr (List_ expr a) -> expr r)
        -> expr r

instance SymList I where
    type List_ I a = [a]

    nil_ _ = I []
    cons_ (I x) (I xs) = I (x:xs)

    caseList_ (I [])     n _ = n
    caseList_ (I (x:xs)) _ c = c (I x) (I xs)

instance Quote q => SymList (Code q) where
    type List_ (Code q) a = [a]

    nil_ _ = [|| [] ||]
    cons_ x xs = [|| $$x : $$xs ||]

    caseList_ xs n c = [|| case $$xs of
        []   -> $$n
        y:ys -> $$(c [|| y ||] [|| ys ||])
        ||]

-------------------------------------------------------------------------------
-- Fun
-------------------------------------------------------------------------------

class SymFun (expr :: k -> Type) where
    type Arr_ expr (a :: k) (b :: k) :: k

    lam_ :: (expr a -> expr b) -> expr (Arr_ expr a b)
    lam' :: (expr a -> expr b) -> expr (Arr_ expr a b)
    app_ :: expr (Arr_ expr a b) -> expr a -> expr b

instance SymFun I where
    type Arr_ I a b = a -> b

    lam_ = coerce
    lam' = coerce
    app_ f x = coerce f x

instance Quote q => SymFun (Code q) where
    type Arr_ (Code q) a b = a -> b

    lam_ f = [|| \  arg -> $$(f [|| arg ||]) ||]
    lam' f = [|| \ !arg -> $$(f [|| arg ||]) ||]
    app_ f x = [|| $$f $$x ||]

-------------------------------------------------------------------------------
-- Unit
-------------------------------------------------------------------------------

class SymUnit (expr :: k -> Type) where
    type Unit_ expr :: k

    unit_ :: expr (Unit_ expr)

instance SymUnit I where
    type Unit_ I = ()

    unit_ = I ()

instance Quote q => SymUnit (Code q) where
    type Unit_ (Code q) = ()

    unit_ = [|| () ||]
