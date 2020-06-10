{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Staged.Stream.Pure.Examples (
    enumFromTo,
    enumFromTo',
    enumFromTo'',
    toZero,
) where

import Prelude hiding (enumFromTo)
import Data.SOP (SOP (..), NS (..), NP (..))

import Staged.Commons
import Staged.Stream.Step
import Staged.Stream.Pure.Combinators (unfold)
import Staged.Stream.Pure.Convenience
import Staged.Stream.Pure.Type

type Singleton a = '[ a ]
type Singleton2 a = Singleton (Singleton a)

enumFromTo :: (IsCode Q Int lo, IsCode Q Int hi) => lo -> hi -> Stream a Int
enumFromTo lo' hi' = MkStream (\_ -> SOP (Z (lo :* Nil))) step where
    lo = toCode lo'
    hi = toCode hi'

    step :: SOP C (Singleton2 Int) -> (Step (C Int) (SOP C (Singleton2 Int)) -> C r) -> C r
    step (SOP (Z (curr :* Nil))) k = toCode [||
        if $$(fromCode curr) > $$(fromCode hi)
        then $$(fromCode $ k Stop)
        else $$(fromCode $ k (Emit curr (SOP (Z (toCode [|| 1 + $$(fromCode curr) ||] :* Nil)))))
        ||]

    -- impossible case
    step (SOP (S ns)) _ = case ns of {}

enumFromTo'
    :: IsCode Q Int lo
    => lo              -- ^ from
    -> Stream Int Int  -- ^ take upper bound as an argument
enumFromTo' lo' = mkStream (\hi -> (lo, hi)) steps where
    lo = toCode lo'

    steps :: (C Int, C Int) -> (Step (C Int) (C Int, C Int) -> C r) -> C r
    steps (curr, hi) k = sIfThenElse
        (toCode [|| (>) ||] @@ curr @@ hi)
        (k Stop)
        (k (Emit curr (toCode [|| 1 + $$(fromCode curr) ||], hi)))

enumFromTo''
    :: IsCode Q Int hi
    => hi              -- ^ from
    -> Stream Int Int  -- ^ take upper bound as an argument
enumFromTo'' hi' = mkStream id steps where
    hi = toCode hi'

    steps :: C Int -> (Step (C Int) (C Int) -> C r) -> C r
    steps curr k = sIfThenElse
        (toCode [|| (>) ||] @@ curr @@ hi)
        (k Stop)
        (k (Emit curr (toCode [|| 1 + $$(fromCode curr) ||])))

-- | Count 'Int's to zero.
toZero :: Stream Int Int
toZero = unfold id $ \curr k -> sIfThenElse
    (toCode [|| (>= 0) ||] @@ curr)
    (k (Just (curr, toCode [|| subtract 1 ||] @@ curr)))
    (k Nothing)
