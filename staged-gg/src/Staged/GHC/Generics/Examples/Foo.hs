{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
{-# OPTIONS_GHC -ddump-splices -dsuppress-module-prefixes #-}
module Staged.GHC.Generics.Examples.Foo where

import qualified GHC.Generics as GHC

import Staged.GHC.Generics

-- | Example product type.
data Foo = Foo [Int] Ordering String
  deriving (GHC.Generic)

$(deriveGeneric ''Foo)

data Bar f a = Bar (f a)
  deriving (GHC.Generic, GHC.Generic1)

$(deriveGeneric ''Bar)
$(deriveGeneric1 ''Bar)
