{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
{-# OPTIONS_GHC -ddump-splices -dsuppress-module-prefixes #-}

{-# LANGUAGE GADTs, InstanceSigs, ScopedTypeVariables #-}
module Staged.GHC.Generics.Examples.Foo where

import qualified GHC.Generics as GHC

import Staged.GHC.Generics

-- temporary
import Staged.GHC.Generics.Examples.Functor
import Data.Proxy (Proxy (..))
import Data.Type.Equality

-------------------------------------------------------------------------------
-- Product type
-------------------------------------------------------------------------------

-- | Example product type.
data Foo = Foo [Int] Ordering String
  deriving (GHC.Generic)

$(deriveGeneric ''Foo)

-------------------------------------------------------------------------------
-- Type with Generic1
-------------------------------------------------------------------------------

data Bar f a = Bar (f a)
  deriving (GHC.Generic, GHC.Generic1)

$(deriveGeneric ''Bar)
$(deriveGeneric1 ''Bar)

-------------------------------------------------------------------------------
-- ECC
-------------------------------------------------------------------------------

data MyGADT a where
    MyGADT1 :: Int :~: a -> Int -> MyGADT a

mkMyGADT1 :: Dict (a ~ Int) -> a -> MyGADT a
mkMyGADT1 Dict = MyGADT1 Refl

castt :: (a ~ b) => Proxy a -> b -> a
castt _ x = x

instance Generic (MyGADT a) where
    type Rep (MyGADT a)
        = (a ~ Int) :==>: K2 a

    to (SuchThat2 (K2 x)) = [|| mkMyGADT1 $$typeEqDict $$x ||]

    from x kont = [|| case $$x of
        MyGADT1 p x -> $$(kont (SuchThat2 (K2 [|| castWith p x ||])))
        -- Could not deduce (Equal a Int) arising from a use of ‘SuchThat2’
        -- 
        -- Rightfully so.
        -- We have Int :~: a, which could be used to give Int ~ a constraint,
        -- which would be lifted to be `CodeC (Int ~ a)`.
        -- It should work out.
        --
        -- N.B. matching on existential in quote should give raise to LiftT
        -- constraint for the new skolem
        ||]
