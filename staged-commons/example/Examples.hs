{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Examples where

import Data.Type.Equality ((:~:) (..))

import Staged.Commons

-------------------------------------------------------------------------------
-- Simple example
-------------------------------------------------------------------------------

ex01 :: IsCode Bool cond => cond -> GHCCode Int
ex01 b = fromCode $ sIfThenElse (toCode b) (sint 1) (sint 2)

-------------------------------------------------------------------------------
-- letrec
-------------------------------------------------------------------------------

fibnr :: Int -> C Int
fibnr = sletrec $ \rec n ->
    if n == 0 then return (sint 1) else
    if n == 1 then return (sint 1) else do
        n1 <- rec (n - 1)
        n2 <- rec (n - 2)
        return $ toCode [|| $$(fromCode n1) + $$(fromCode n2) ||]

-------------------------------------------------------------------------------
-- heterogeneous letrec
-------------------------------------------------------------------------------

data Even = Zero | E1 Odd deriving Show
data Odd  = O1 Even       deriving Show

data Tag a where
    TagE :: Tag (Int -> Maybe Even)
    TagO :: Tag (Int -> Maybe Odd)

eqTag :: Tag a -> Tag b -> Maybe (a :~: b)
eqTag TagE TagE = Just Refl
eqTag TagE TagO = Nothing
eqTag TagO TagE = Nothing
eqTag TagO TagO = Just Refl

isEvenCodeH :: Tag sig -> C sig
isEvenCodeH = sletrecH eqTag $ \rec tag -> case tag of
    TagE -> do
        isOdd <- rec TagO
        return $ toCode [|| \n -> case n of
                                0 -> Just Zero
                                _ -> E1 <$> $$(fromCode isOdd) (n - 1)
                         ||]

    TagO -> do
        isEven <- rec TagE
        return $ toCode [|| \n -> case n of
                                0 -> Nothing
                                _ -> O1 <$> $$(fromCode isEven) (n - 1)
                         ||]
