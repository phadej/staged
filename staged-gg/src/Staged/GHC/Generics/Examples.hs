{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wall -Wno-orphans#-}
{-# OPTIONS_GHC -ddump-splices #-}

module Staged.GHC.Generics.Examples (
    -- * Examples of derivng type-classes
    module Staged.GHC.Generics.Examples.Functor,
    module Staged.GHC.Generics.Examples.Monoid,
    -- * Example of defining instances for data types
    module Staged.GHC.Generics.Examples.Foo,
    -- * Usage
    mapCompose, mapConst, mapIdentity, mapSum, mapProduct,
) where

import Staged.GHC.Generics.Examples.Foo
import Staged.GHC.Generics.Examples.Functor
import Staged.GHC.Generics.Examples.Monoid

import Data.Functor.Compose  (Compose (..))
import Data.Functor.Const    (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Product  (Product (..))
import Data.Functor.Sum      (Sum (..))

-------------------------------------------------------------------------------
-- Semigroup and Monoid
-------------------------------------------------------------------------------

instance Semigroup Foo where
    x <> y = $$(gmappend [|| x ||] [|| y ||])

instance Monoid Foo where
    mempty = $$(gmempty)

-------------------------------------------------------------------------------
-- Functor
-------------------------------------------------------------------------------

mapCompose :: (a -> b) -> Compose [] Maybe a -> Compose [] Maybe b
mapCompose f x = $$(genericFmap [|| f ||] [|| x ||])

mapConst :: (a -> b) -> Const c a -> Const c b
mapConst f x = $$(genericFmap [|| f ||] [|| x ||])

mapIdentity :: (a -> b) -> Identity a -> Identity b
mapIdentity f x = $$(genericFmap [|| f ||] [|| x ||])

mapSum :: (a -> b) -> Sum [] Maybe a -> Sum [] Maybe b
mapSum f x = $$(genericFmap [|| f ||] [|| x ||])

mapProduct :: (a -> b) -> Product [] Maybe a -> Product [] Maybe b
mapProduct f x = $$(genericFmap [|| f ||] [|| x ||])
