{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Data.SOP.Fn.Cons (
    -- * Proxies
    prCons,
    prHead,
    prTail,
    ) where

import Data.Proxy

-- this is not good place, but good enough
prCons :: proxy x -> proxy' xs -> Proxy (x ': xs)
prCons _ _ = Proxy

prHead :: proxy (x ': xs) -> Proxy x
prHead _ = Proxy

prTail :: proxy (x ': xs) -> Proxy xs
prTail _ = Proxy
