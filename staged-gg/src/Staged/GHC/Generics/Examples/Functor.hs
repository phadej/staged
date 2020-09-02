{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE EmptyCase        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

-- A price for CodeC
{-# LANGUAGE UndecidableInstances #-}

-- Further experiments
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
module Staged.GHC.Generics.Examples.Functor where

import Data.Coerce (coerce)
import Data.Kind (Constraint, Type)

import Staged.GHC.Generics

-------------------------------------------------------------------------------
-- mempty
-------------------------------------------------------------------------------

genericFmap :: (Generic1 f, GFunctor (Rep1 f), Quote q) => Code q (a -> b) -> Code q (f a) -> Code q (f b)
genericFmap f x = from1 x $ \x' -> to1 (gmap f x')

class GFunctor f where
    gmap :: Quote q => Code q (a -> b) -> f (Code q) a -> f (Code q) b

instance GFunctor f => GFunctor (M2 i c f) where
    gmap f (M2 x) = M2 (gmap f x)

instance (GFunctor f, CodeC (Functor g)) => GFunctor (f :@@: g) where
    gmap f (App2 x) = App2 $ gmap (sfmap f) x

instance (GFunctor f, GFunctor g) => GFunctor (f :**: g) where
    gmap f (x :**: y) = gmap f x :**: gmap f y

instance (GFunctor f, GFunctor g) => GFunctor (f :++: g) where
    gmap f (L2 x) = L2 (gmap f x)
    gmap f (R2 y) = R2 (gmap f y)

instance GFunctor (K2 c) where
    gmap _ (K2 c) = K2 c

instance GFunctor U2 where
    gmap _ = coerce

instance GFunctor V2 where
    gmap _ = coerce

instance GFunctor Par2 where
    gmap f (Par2 x) = Par2 [|| $$f $$x ||]

-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------

-- | HACK
type family CodeC (a :: Constraint) :: Constraint
type instance CodeC (Functor f) = CodeFunctor f

class CodeFunctor f where
    sfmap :: Quote q => Code q (a -> b) -> Code q (f a -> f b)

-- somewhat boring instances
instance CodeFunctor [] where
    sfmap f = [|| fmap @[] $$f ||]

instance CodeFunctor Maybe where
    sfmap f = [|| fmap @Maybe $$f ||]

-------------------------------------------------------------------------------
-- ==>
-------------------------------------------------------------------------------

class a ~ b => Equal a b where
    typeEqDict :: Quote q => Code q (Dict (a ~ b))
    typeEqDict = [|| Dict ||]

type instance CodeC (a ~ b) = Equal a b

class CodeEq a where
    eqCodeDict :: Quote q => Code q (Dict (Eq a))

    sequal :: Quote q => Code q (a -> a -> Bool)

instance CodeEq Int where
    eqCodeDict = [|| Dict ||]
    sequal = [|| (==) @Int ||]

type instance CodeC (Eq a) = CodeEq a

data Dict (c :: Constraint) where
    Dict :: c => Dict c

-- https://ryanglscott.github.io/2018/02/11/how-to-derive-generic-for-some-gadts/

data ((c :: Constraint) :==>: (f :: (Type -> Type) -> k -> *)) (q :: Type -> Type) (a :: k) where
    SuchThat2 :: CodeC c => f q a -> (c :==>: f) q a

genericEq :: (Generic a, GEq (Rep a), Quote q) => Code q a -> Code q a -> Code q Bool
genericEq x y = from x $ \x' -> from y $ \y' -> geq x' y'

class GEq f where
    geq :: Quote q => f (Code q) a -> f (Code q) a -> Code q Bool

instance GEq f => GEq (M2 i c f) where
    geq (M2 x) (M2 y) = geq x y

instance (GEq f, GEq g) => GEq (f :**: g) where
    geq (x1 :**: x2) (y1 :**: y2) = [|| $$(geq x1 y1) && $$(geq x2 y2) ||]

instance (CodeC c => GEq f) => GEq (c :==>: f) where
    geq (SuchThat2 x) (SuchThat2 y) = geq x y

instance CodeC (Eq a) => GEq (K2 a) where
    geq (K2 x) (K2 y) = [|| $$sequal $$x $$y ||]
