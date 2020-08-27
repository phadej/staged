{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}
module Staged.GHC.Generics.Examples.Monoid where

import Staged.GHC.Generics

-------------------------------------------------------------------------------
-- mempty
-------------------------------------------------------------------------------

gmempty :: (Generic a, GMempty (Rep a), Quote q) => Code q a
gmempty = to gmempty'

class GMempty f where
    gmempty' :: Quote q => f (Code q) x

instance GMempty f => GMempty (M2 i c f) where
    gmempty' = M2 gmempty'

instance (GMempty f, GMempty g) => GMempty (f :**: g) where
    gmempty' = gmempty' :**: gmempty'

instance Monoid a => GMempty (K2 a) where
    gmempty' = K2 [|| mempty ||]

instance GMempty U2 where
    gmempty' = U2

-------------------------------------------------------------------------------
-- mappend
-------------------------------------------------------------------------------

gmappend :: (Generic a, GMappend (Rep a), Quote q) => Code q a -> Code q a -> Code q a
gmappend x y =
    from x $ \x' ->
    from y $ \y' ->
    to (gmappend' x' y')

class GMappend f where
    gmappend' :: Quote q => f (Code q) x -> f (Code q) x -> f (Code q) x

instance GMappend f => GMappend (M2 i c f) where
    gmappend' (M2 x) (M2 y) = M2 (gmappend' x y)

instance (GMappend f, GMappend g) => GMappend (f :**: g) where
    gmappend' (x1 :**: x2) (y1 :**: y2) = gmappend' x1 y1 :**: gmappend' x2 y2

instance Semigroup a => GMappend (K2 a) where
    gmappend' (K2 x) (K2 y) = K2 [|| $$x <> $$y ||]

instance GMappend U2 where
    gmappend' U2 U2 = U2
