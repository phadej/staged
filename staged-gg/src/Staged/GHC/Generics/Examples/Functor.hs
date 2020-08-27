{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE EmptyCase        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}
module Staged.GHC.Generics.Examples.Functor where

import Staged.GHC.Generics
import Data.Coerce (coerce)

-------------------------------------------------------------------------------
-- mempty
-------------------------------------------------------------------------------

genericFmap :: (Generic1 f, GFunctor (Rep1 f), Quote q) => Code q (a -> b) -> Code q (f a) -> Code q (f b)
genericFmap f x = from1 x $ \x' -> to1 (gmap f x')

class GFunctor f where
    gmap :: Quote q => Code q (a -> b) -> f (Code q) a -> f (Code q) b

instance GFunctor f => GFunctor (M2 i c f) where
    gmap f (M2 x) = M2 (gmap f x)

instance (GFunctor f, Functor g) => GFunctor (f :@@: g) where
    gmap f (App2 x) = App2 $ gmap [|| fmap $$f ||] x

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
