{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Data.SOP.Sh where

import Data.Kind (Constraint, Type)
import Generics.SOP

type Sh = Sh' Top

-- | The ultimate singleton for lists.
data Sh' :: (k -> Constraint) -> [k] -> Type where
    ShNil  :: Sh' c '[]
    ShCons :: (c z, All c zs)
           => {-# UNPACK #-} !(Proxy z)
           -> {-# UNPACK #-} !(Proxy zs)
           -> Sh' c (z ': zs)

sh :: forall xs c proxy. All c xs => proxy xs -> Sh' c xs
sh _ = case sList :: SList xs of
    SNil  -> ShNil
    SCons -> ShCons Proxy Proxy

shTop :: SListI xs => proxy xs -> Sh xs
shTop = sh

sh2 :: forall xss c proxy. All2 c xss => proxy xss -> Sh' (All c) xss
sh2 _ = case sList :: SList xss of
    SNil  -> ShNil
    SCons -> ShCons Proxy Proxy

shAll :: SListI2 xss => proxy xss -> Sh' SListI xss
shAll = sh2

toAll :: Sh' c xs -> (All c xs => r) -> r
toAll ShNil        k = k
toAll (ShCons _ _) k = k
