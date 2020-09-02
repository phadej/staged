{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wall -Wno-orphans#-}
{-# OPTIONS_GHC -ddump-splices #-}

-- Experimentation
{-# LANGUAGE GADTs #-}

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

--
--    genericFmap [|| f_ajf4 ||] [|| x_ajf5 ||]
--  ======>
--    case x_ajf5 of {
--      Compose f1_ajmR
--        -> Compose (((fmap @[]) ((fmap @Maybe) f_ajf4)) f1_ajmR) }
--
-- Note the type applications!
mapCompose :: (a -> b) -> Compose [] Maybe a -> Compose [] Maybe b
mapCompose f x = $$(genericFmap [|| f ||] [|| x ||])

-- this still doesn't work
-- No instance for (CodeFunctor f)
-- :(((
-- mapCompose' :: CodeFunctor f => (a -> b) -> Compose f Maybe a -> Compose f Maybe b
-- mapCompose' f x = $$(genericFmap [|| f ||] [|| x ||])

mapConst :: (a -> b) -> Const c a -> Const c b
mapConst f x = $$(genericFmap [|| f ||] [|| x ||])

mapIdentity :: (a -> b) -> Identity a -> Identity b
mapIdentity f x = $$(genericFmap [|| f ||] [|| x ||])

mapSum :: (a -> b) -> Sum [] Maybe a -> Sum [] Maybe b
mapSum f x = $$(genericFmap [|| f ||] [|| x ||])

mapProduct :: (a -> b) -> Product [] Maybe a -> Product [] Maybe b
mapProduct f x = $$(genericFmap [|| f ||] [|| x ||])

-------------------------------------------------------------------------------
-- ECC
-------------------------------------------------------------------------------

-- Constraint solver should be able to deduce facts like:
--
--    • Could not deduce (CodeEq a) arising from a use of ‘genericEq’
--      from the context: CodeC (a ~ Int)
--
-- Is there
--
--       a => b
-- ------------------
-- CodeC a => CodeC b
--
-- rule?
--
instance Eq (MyGADT a) where
    x == y = $$(genericEq [|| x ||] [|| y ||])
