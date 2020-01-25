{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE EmptyCase               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-- | More convenient (than direct) 'Stream' creation.
module Staged.Stream.Pure.Convenience (
    -- * Convenience helpers:
    mkStream,
    -- * Type families
    FlattenCode, FlattenCode0 (..), FlattenCode1, FlattenCode2,
    FlattenCodeErr, FlattenCodeErr0, FlattenCodeErr1, FlattenCodeErr2,
    Description,
    ) where

import Data.Kind           (Constraint, Type)
import GHC.TypeLits        (ErrorMessage (..), TypeError)
import Language.Haskell.TH (Q, TExp)

import Data.SOP
import qualified Generics.SOP as SOP

import Data.SOP.Fn.All
import Data.SOP.Fn.Flatten
import Staged.Commons
import Staged.Stream.Pure.Type

type Description a = SOP.Code a

-- | Create 'Stream' from a simple state.
--
-- __Example:__ definition of 'Staged.Stream.Pure.Combinators.mapPipe'.
-- There are two states: the initial one and the "stopped" one, which
-- stream moves into after yielding a value.
-- This state space is representable with 'Maybe':
--
-- @
-- 'Staged.Stream.Pure.Combinators.mapPipe' :: forall a b. ('C' a -> 'C' b) -> 'Stream' a b
-- 'Staged.Stream.Pure.Combinators.mapPipe' f = 'mkStream' start step where
--     start :: 'C' a -> 'Maybe' ('C' b)
--     start a = 'Just' ('C' (f a))
--
--     step :: 'Maybe' ('C' b) -> ('Step' ('C' b) ('Maybe' ('C' b)) -> 'C' r) -> 'C' r
--     step 'Nothing'      k = k 'Stop'
--     step ('Just' ('C' b)) k = k ('Emit' b 'Nothing')
-- @
--
-- /Note:/ you should prefer using 'Staged.Stream.Pure.Combinators.map' combinator if
-- possible, as it doesn't make state space bigger.
--
mkStream
    :: forall a b s xssss. (SOP.Generic s, FlattenCodeErr s, FlattenCode s xssss)
    => (C a -> s)                                             -- ^ start state
    -> (forall r. s -> (Step (C b) s -> C r) -> C r)  -- ^ step function
    -> Stream a b
mkStream start0 step0 =
    allFLATTEN prTop (Proxy :: Proxy xssss) $ MkStream start step
  where
    start :: C a -> SOP C (FLATTEN xssss)
    start = from' . start0

    step :: SOP C (FLATTEN xssss) -> (Step (C b) (SOP C (FLATTEN xssss)) -> C r) -> C r
    step s k = step0 (to' s) (k . fmap from')

    from' :: s -> SOP C (FLATTEN xssss)
    from' = SOP . flatten_NSNP . codeFlatFwd . unSOP . SOP.from

    to' :: SOP C (FLATTEN xssss) -> s
    to' = SOP.to . SOP . codeFlatBwd . unflatten_NSNP . unSOP

-------------------------------------------------------------------------------
-- Type families
-------------------------------------------------------------------------------

type FlattenCode    s = FlattenCode0 (Description s)
type FlattenCodeErr s = FlattenCodeErr0 (Description s)

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
