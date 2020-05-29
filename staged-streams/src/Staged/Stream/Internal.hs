{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE EmptyCase               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Staged.Stream.Internal where

import Data.Kind           (Constraint, Type)
import Data.Proxy          (Proxy (..))
import Data.SOP            (I (..), NP (..), NS (..), SListI, SListI2, SOP (..), unSOP, unI, unZ)
import GHC.TypeLits        (ErrorMessage (..), TypeError)
import Language.Haskell.TH (Q, TExp)

import qualified GHC.Generics as GHC

import Data.SOP.Fn.All
import Data.SOP.Fn.Flatten
import Staged.Commons

-------------------------------------------------------------------------------
-- Type families
-------------------------------------------------------------------------------

class FlattenCode s xss | s -> xss where
    allFlattenCode :: Proxy s -> (SListI2 xss => r) -> r

    from' :: s -> SOP C xss
    to'   :: SOP C xss -> s

instance {-# OVERLAPPING #-} m ~ Q => FlattenCode (Code m a) '[ '[ a ] ] where
    allFlattenCode _ k = k

    from' x = SOP (Z (x :* Nil))
    to' (SOP (Z (x :* Nil))) = x
    to' (SOP (S x))          = case x of {}

instance {-# OVERLAPPABLE #-} (GHC.Generic s, GFrom s, GTo s, FlattenCodeErr0 (GCode s), FlattenCode0 (GCode s) xssss, xss ~ FLATTEN xssss) => FlattenCode s xss where
    allFlattenCode _ = allFLATTEN prTop (Proxy :: Proxy xssss)

    from' = SOP . flatten_NSNP . codeFlatFwd . unSOP . gfrom
    to'   = gto . SOP . codeFlatBwd . unflatten_NSNP . unSOP

-- type FlattenCode    s = FlattenCode0 (Description s)
-- type FlattenCodeErr s = FlattenCodeErr0 (Description s)

class SListI4 flat => FlattenCode0 (code :: [[Type]]) (flat :: [[[[Type]]]]) | code -> flat where
    codeFlatFwd :: NS (NP I) code -> NS (NP (NS (NP C))) flat
    codeFlatBwd :: NS (NP (NS (NP C))) flat -> NS (NP I) code

type family FlattenCodeErr0 (code :: [[Type]]) :: Constraint where
    FlattenCodeErr0 '[]       = ()
    FlattenCodeErr0 (x ': xs) = (FlattenCodeErr1 x, FlattenCodeErr0 xs)

instance FlattenCode0 '[] '[] where
    codeFlatFwd ns = case ns of {}
    codeFlatBwd ns = case ns of {}

instance (FlattenCode1 xs ys, FlattenCode0 xss yss) => FlattenCode0 (xs ': xss) (ys ': yss) where
    codeFlatFwd (Z x)  = Z (codeFlatFwd1 x)
    codeFlatFwd (S xs) = S (codeFlatFwd xs)

    codeFlatBwd (Z y)  = Z (codeFlatBwd1 y)
    codeFlatBwd (S ys) = S (codeFlatBwd ys)

class SListI3 flat => FlattenCode1 (code :: [Type]) (flat :: [[[Type]]]) | code -> flat where
    codeFlatFwd1 :: NP I code -> NP (NS (NP C)) flat
    codeFlatBwd1 :: NP (NS (NP C)) flat -> NP I code

type family FlattenCodeErr1 (code :: [Type]) :: Constraint where
    FlattenCodeErr1 '[]       = ()
    FlattenCodeErr1 (x ': xs) = (FlattenCodeErr2 x, FlattenCodeErr1 xs)

instance FlattenCode1 '[] '[] where
    codeFlatFwd1 Nil = Nil
    codeFlatBwd1 Nil = Nil

instance (FlattenCode2 x y, FlattenCode1 xs ys) => FlattenCode1 (x ': xs) (y ': ys) where
    codeFlatFwd1 (x :* xs) = codeFlatFwd2 x :* codeFlatFwd1 xs
    codeFlatBwd1 (y :* ys) = codeFlatBwd2 y :* codeFlatBwd1 ys

class SListI2 flat => FlattenCode2 (code :: Type) (flat :: [[Type]]) | code -> flat where
    codeFlatFwd2 :: I code -> NS (NP C) flat
    codeFlatBwd2 :: NS (NP C) flat -> I code

type family FlattenCodeErr2 (code :: Type) :: Constraint where
    FlattenCodeErr2 (SOP f xss)  = ()
    FlattenCodeErr2 (C x)        = ()
    FlattenCodeErr2 (NP f xs)    = ()
    FlattenCodeErr2 (Q (TExp x)) = ()
    FlattenCodeErr2 x = TypeError ('Text "FlattenCodeErr: Something is not code " ':<>: 'ShowType x)

instance (SListI2 xss, f ~ C) => FlattenCode2 (SOP f xss) xss where
    codeFlatFwd2 = unSOP . unI
    codeFlatBwd2 = I . SOP

instance (SListI xs, f ~ C) => FlattenCode2 (NP f xs) '[ xs ] where
    codeFlatFwd2 = Z . unI
    codeFlatBwd2 = I . unZ

instance FlattenCode2 (C x) '[ '[ x ] ] where
    codeFlatFwd2 (I x) = Z (x :* Nil)
    codeFlatBwd2 (Z (x :* Nil)) = I x
    codeFlatBwd2 (S ns)         = case ns of {}

instance (e ~ TExp x) => FlattenCode2 (Q e) '[ '[ x ] ] where
    codeFlatFwd2 (I x) = Z (C x :* Nil)
    codeFlatBwd2 (Z (C x :* Nil)) = I x
    codeFlatBwd2 (S ns)            = case ns of {}

-------------------------------------------------------------------------------
-- GGP
-------------------------------------------------------------------------------

-- this code is extracted from generics-sop library

-- Copyright (c) 2014-2015, Well-Typed LLP, Edsko de Vries, Andres Löh
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
-- 
-- 1. Redistributions of source code must retain the above copyright notice,
--    this list of conditions and the following disclaimer.
-- 
-- 2. Redistributions in binary form must reproduce the above copyright notice,
--    this list of conditions and the following disclaimer in the documentation
--    and/or other materials provided with the distribution.
-- 
-- 3. Neither the name of the copyright holder nor the names of its contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-------------------------------------------------------------------------------
-- GGP: Code
-------------------------------------------------------------------------------

type family ToSingleCode (a :: Type -> Type) :: Type where
    ToSingleCode (GHC.K1 _i a) = a

type family ToProductCode (a :: Type -> Type) (xs :: [Type]) :: [Type] where
    ToProductCode (a GHC.:*: b)       xs = ToProductCode a (ToProductCode b xs)
    ToProductCode GHC.U1              xs = xs
    ToProductCode (GHC.M1 GHC.S _c a) xs = ToSingleCode a ': xs

type family ToSumCode (a :: Type -> Type) (xs :: [[Type]]) :: [[Type]] where
    ToSumCode (a GHC.:+: b)       xs = ToSumCode a (ToSumCode b xs)
    ToSumCode GHC.V1              xs = xs
    ToSumCode (GHC.M1 GHC.D _c a) xs = ToSumCode a xs
    ToSumCode (GHC.M1 GHC.C _c a) xs = ToProductCode a '[] ': xs

-------------------------------------------------------------------------------
-- GGP: From
-------------------------------------------------------------------------------

class GSingleFrom (a :: Type -> Type) where
    gSingleFrom :: a x -> ToSingleCode a

instance GSingleFrom (GHC.K1 i a) where
    gSingleFrom (GHC.K1 a) = a

class GProductFrom (a :: Type -> Type) where
    gProductFrom :: a x -> NP I xs -> NP I (ToProductCode a xs)

instance (GProductFrom a, GProductFrom b) => GProductFrom (a GHC.:*: b) where
    gProductFrom (a GHC.:*: b) xs = gProductFrom a (gProductFrom b xs)

instance GProductFrom GHC.U1 where
    gProductFrom GHC.U1 xs = xs

instance GSingleFrom a => GProductFrom (GHC.M1 GHC.S c a) where
    gProductFrom (GHC.M1 a) xs = I (gSingleFrom a) :* xs

class GSumFrom (a :: Type -> Type) where
    gSumFrom :: forall x xss. a x -> Proxy xss -> SOP I (ToSumCode a xss)
    gSumSkip :: proxy a -> SOP I xss -> SOP I (ToSumCode a xss)

instance GSumFrom GHC.V1 where
    gSumFrom x = case x of {}
    gSumSkip _ xss = xss

instance (GSumFrom a, GSumFrom b) => GSumFrom (a GHC.:+: b) where
    gSumFrom (GHC.L1 a) xss = gSumFrom a (toSumCodeProxy xss) where
        toSumCodeProxy :: Proxy xss -> Proxy (ToSumCode b xss)
        toSumCodeProxy _ = Proxy

    gSumFrom (GHC.R1 b) xss = gSumSkip (Proxy :: Proxy a) (gSumFrom b xss)

    gSumSkip _ xss = gSumSkip (Proxy :: Proxy a) (gSumSkip (Proxy :: Proxy b) xss)

instance (GSumFrom a) => GSumFrom (GHC.M1 GHC.D c a) where
    gSumFrom (GHC.M1 a) xss = gSumFrom a xss
    gSumSkip _      xss = gSumSkip (Proxy :: Proxy a) xss

instance (GProductFrom a) => GSumFrom (GHC.M1 GHC.C c a) where
    gSumFrom (GHC.M1 a) _    = SOP (Z (gProductFrom a Nil))
    gSumSkip _ (SOP xss) = SOP (S xss)

-------------------------------------------------------------------------------
-- GGP: To
-------------------------------------------------------------------------------

class GSingleTo (a :: Type -> Type) where
    gSingleTo :: ToSingleCode a -> a x

instance GSingleTo (GHC.K1 i a) where
    gSingleTo a = GHC.K1 a

class GProductTo (a :: Type -> Type) where
    gProductTo :: NP I (ToProductCode a xs) -> (a x -> NP I xs -> r) -> r

instance (GProductTo a, GProductTo b) => GProductTo (a GHC.:*: b) where
    gProductTo xs k = gProductTo xs (\ a ys -> gProductTo ys (\ b zs -> k (a GHC.:*: b) zs))

instance GSingleTo a => GProductTo (GHC.M1 GHC.S c a) where
    gProductTo (I a :* xs) k = k (GHC.M1 (gSingleTo a)) xs

instance GProductTo GHC.U1 where
    gProductTo xs k = k GHC.U1 xs

class GSumTo (a :: Type -> Type) where
    gSumTo :: SOP I (ToSumCode a xss) -> (a x -> r) -> (SOP I xss -> r) -> r

instance GSumTo GHC.V1 where
    gSumTo x _ k = k x

instance (GSumTo a, GSumTo b) => GSumTo (a GHC.:+: b) where
    gSumTo xss s k = gSumTo xss (s . GHC.L1) (\ r -> gSumTo r (s . GHC.R1) k)

instance (GProductTo a) => GSumTo (GHC.M1 GHC.C c a) where
    gSumTo (SOP (Z xs)) s _ = s (GHC.M1 (gProductTo xs ((\ x Nil -> x) :: a x -> NP I '[] -> a x)))
    gSumTo (SOP (S xs)) _ k = k (SOP xs)

instance (GSumTo a) => GSumTo (GHC.M1 GHC.D c a) where
    gSumTo xss s k = gSumTo xss (s . GHC.M1) k

-------------------------------------------------------------------------------
-- GGP: Higher level
-------------------------------------------------------------------------------

type GCode (a :: Type) = ToSumCode (GHC.Rep a) ('[] :: [[Type]])
type GFrom a = GSumFrom (GHC.Rep a)
type GTo a = GSumTo (GHC.Rep a)

gfrom :: (GFrom a, GHC.Generic a) => a -> SOP I (GCode a)
gfrom x = gSumFrom (GHC.from x) (Proxy :: Proxy '[])

gto :: forall a. (GTo a, GHC.Generic a) => SOP I (GCode a) -> a
gto x = GHC.to (gSumTo x id ((\y -> case y of {}) :: SOP I '[] -> (GHC.Rep a) x))
