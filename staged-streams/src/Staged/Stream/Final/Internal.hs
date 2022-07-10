{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE EmptyCase               #-}
{-# LANGUAGE ExplicitNamespaces      #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -fprint-explicit-kinds -fprint-explicit-foralls #-}
module Staged.Stream.Final.Internal where

import Data.Kind           (Type, Constraint)
import Data.Bifunctor (bimap)
import Data.Proxy          (Proxy (..))

import qualified GHC.Generics as GHC
import Data.Type.Equality ((:~:) (..))

import Data.Monoid (Ap (..))
import Data.SOP (Top,(:.:) (..), unZ, K (..), SList (..), SListI2, SListI, sList, SOP (..), NP (..), NS (..), unSOP)
import Data.SOP.NP (cmap_NP, sequence'_NP, map_NP)
import Data.SOP.NS (collapse_NS, liftA2_NS)

import Data.SOP.Fn.Append
import Data.SOP.Fn.MapCons
import Data.SOP.Fn.ConcatMapAppend

import Data.GADT.Compare
import Symantics

singSOP :: f a -> SOP f '[ '[ a ] ]
singSOP x = SOP (Z (x :* Nil))

unsingSOP :: SOP f '[ '[ a ] ] -> f a
unsingSOP (SOP (unZ -> (x :* Nil))) = x

-------------------------------------------------------------------------------
-- Type families
-------------------------------------------------------------------------------

-- | A utility type-class powering convenience stream creation functions.
class FlattenCode (s :: (k -> Type) -> Type) (term :: k -> Type) (xss :: [[k]]) | s term -> xss where
    allFlattenCode :: Proxy s -> Proxy term -> (SListI2 xss => r) -> r

    from' :: s term -> SOP term xss
    to'   :: SOP term xss -> s term

flattenCodeKind
    :: forall {k} (s :: (k -> Type) -> Type) (term :: k -> Type) (xss :: [[k]]). FlattenCode s term xss
    => Proxy s -> Proxy xss -> Proxy k
flattenCodeKind _ _ = Proxy

-------------------------------------------------------------------------------
-- new impl
-------------------------------------------------------------------------------

instance (GHC.Generic (s term), FlattenCodeRep (GHC.Rep (s term)) term xss) => FlattenCode (s :: (k -> Type) -> Type) (term :: (k -> Type)) (xss :: [[k]]) where
    allFlattenCode _ term k = allFlattenCodeRep (Proxy @(GHC.Rep (s term))) term k

    from' = fromRep . GHC.from
    to'   = GHC.to . toRep

class FlattenCodeRep rep term (xss :: [[k]]) | rep term -> xss where
    allFlattenCodeRep :: Proxy rep -> Proxy term -> (SListI2 xss => r) -> r

    fromRep :: rep () -> SOP term xss
    toRep   :: SOP term xss -> rep ()

instance FlattenCodeRep f term xss => FlattenCodeRep (GHC.M1 i c f) term xss where
    allFlattenCodeRep _ term k = allFlattenCodeRep (Proxy @f) term k

    fromRep = fromRep . GHC.unM1
    toRep   = GHC.M1 . toRep

instance forall k (f :: Type -> Type) (g :: Type -> Type) (term :: k -> Type) (xss :: [[k]]) (yss :: [[k]]) (zss :: [[k]]). (FlattenCodeRep f term xss, FlattenCodeRep g term yss, zss ~ Append xss yss) => FlattenCodeRep (f GHC.:+: g) term zss where
    allFlattenCodeRep _ term k =
        allFlattenCodeRep (Proxy @f) term $
        allFlattenCodeRep (Proxy @g) term $
        append_SListI2 (Proxy @xss) (Proxy @yss) k

    fromRep =
        allFlattenCodeRep (Proxy @f) (Proxy @term) $
        append_SOP . bimap fromRep fromRep . toEither
      where
        toEither :: (f GHC.:+: g) x -> Either (f x) (g x)
        toEither (GHC.L1 x) = Left x
        toEither (GHC.R1 y) = Right y

    toRep =
        allFlattenCodeRep (Proxy @f) (Proxy @term) $
        fromEither . bimap toRep toRep . split_SOP
      where
        fromEither :: Either (f x) (g x) -> (f GHC.:+: g) x
        fromEither = either GHC.L1 GHC.R1

instance forall k (f :: Type -> Type) (g :: Type -> Type) (term :: k -> Type) (xss :: [[k]]) (yss :: [[k]]) (zss :: [[k]]). (FlattenCodeRep f term xss, FlattenCodeRep g term yss, zss ~ ConcatMapAppend xss yss) => FlattenCodeRep (f GHC.:*: g) term zss where
    allFlattenCodeRep _ term k =
        allFlattenCodeRep (Proxy @f) term $
        allFlattenCodeRep (Proxy @g) term $
        concatMapAppend_SListI2 (Proxy @xss) (Proxy @yss) k

    fromRep (xss GHC.:*: yss) =
        allFlattenCodeRep (Proxy @f) (Proxy @term) $
        allFlattenCodeRep (Proxy @g) (Proxy @term) $
        concatMapAppend_SOP (fromRep xss) (fromRep yss)

    toRep sop =
        allFlattenCodeRep (Proxy @f) (Proxy @term) $
        allFlattenCodeRep (Proxy @g) (Proxy @term) $
        let (xss, yss) = unconcatMapAppend_SOP sop
        in toRep xss GHC.:*: toRep yss

instance forall k (code :: k -> Type) (xss :: [[k]]). xss ~ '[ '[] ] => FlattenCodeRep GHC.U1 code xss where
    allFlattenCodeRep _ _ k = k

    fromRep _ = SOP (Z Nil)
    toRep _   = GHC.U1

instance (FlattenCodeK a term xss, r ~ GHC.R) => FlattenCodeRep (GHC.K1 r a) term xss where
    allFlattenCodeRep _ _ k = k

    fromRep = fromK . GHC.unK1
    toRep   = GHC.K1 . toK

type FlattenCodeK :: forall k. Type -> (k -> Type) -> [[k]] -> Constraint
class SListI2 xss => FlattenCodeK a term xss | a -> xss where
    fromK :: a -> SOP term xss
    toK   :: SOP term xss -> a

instance forall k (code :: k -> Type) (code' :: k -> Type) (xss :: [[k]]). (code ~ code', SListI2 xss) => FlattenCodeK (SOP code xss) code' xss where
    fromK = id
    toK = id

instance forall k (code :: k -> Type) (code' :: k -> Type) (a :: k). code ~ code' => FlattenCodeK (Ap code a) code' '[ '[ a ] ] where
    fromK = singSOP . getAp
    toK = Ap . unsingSOP

-------------------------------------------------------------------------------
-- Fixedpoints
-------------------------------------------------------------------------------

-- | Type of 'fix'. Fixedpoint of @a@.
type Fixedpoint a = (a -> a) -> a

termLetRec_SOP
    :: forall xss b code. (SListI2 xss, SymLetRec code, SymFun code)
    => Fixedpoint (SOP code xss -> code b)
termLetRec_SOP f x = termLetRec_NSNP_alt (\y z -> f (y . unSOP) (SOP z)) (unSOP x)

-- | 'termLetRec_SOP' with additional argument in each state.
termLetRec1_SOP
    :: forall xss b c code. (SListI2 xss, SymLetRec code, SymFun code)
    => Fixedpoint (SOP code xss -> code b -> code c)
termLetRec1_SOP f sop b =
    allMapCons (Proxy @Top) (Proxy @b) (Proxy @xss) $
    termLetRec_SOP
        (\rec x -> case unmapCons_SOP x of
            ~(b', sop') -> f (\sop'' b'' -> rec (mapCons_SOP b'' sop'')) sop' b')
        (mapCons_SOP b sop)

-------------------------------------------------------------------------------
-- Alternative sletrec_NSNP using sletrec
-------------------------------------------------------------------------------

-- I'm not proud of this.
--
-- But inspection-testing doesn't say this is bad
-- (other use sletrec1_NSNP)
--
-- This is implementation of 'sletrec_NSNP' using 'sletrec',
-- i.e. without using unsafeTExpCoerce.
--
-- This shows that sletrec is good enough.
-- (The generated code looks just horrible).
--
termLetRec_NSNP_alt
    :: forall {k} (xss :: [[k]]) b code. (SListI2 xss, SymLetRec code, SymFun code)
    => Fixedpoint (NS (NP code) xss -> code b)
termLetRec_NSNP_alt body args = withNSNP args $ \el f ->
    f (letrec_ loopid el)
  where
    -- because of simplified subsumption
    loopid
        :: Monad m
        => (forall c. Elem code b xss c -> m (code c))
        -> Elem code b xss d -> m (code d)
    loopid eta = loop id eta

    loop :: forall m (yss :: [[k]]). Monad m
         => (NS (NP code) yss -> NS (NP code) xss)
         -> (forall c. Elem code b xss c -> m (code c))
         -> (forall d. Elem code b yss d -> m (code d))
    loop mk rec Here = do
        let fn :: Pair code b xss xs -> (:.:) m (FunTo code b) xs
            fn (Pair el kont) = Comp $ do
                f <- rec el
                return (FunTo (kont f))

        let pop' :: NP (m :.: FunTo code b) xss
            pop' = cmap_NP (Proxy :: Proxy SListI) fn (pairs :: NP (Pair code b xss) xss)

        pop <- sequence'_NP pop'

        let rec' :: NS (NP code) xss -> code b
            rec' ns = collapse_NS (liftA2_NS (\(FunTo f) np -> K (f np)) pop ns)

        return $ slam_NP' $ \np -> body rec' (mk (Z np))

    loop mk rec (There next) = do
        loop (mk . S) rec next

-- Curry type-family with utilities
type family Curry (term :: k -> Type) (r :: k) (xs :: [k]) :: k where
    Curry term r '[]      = r
    Curry term r (x : xs) = Arr_ term x (Curry term r xs)

-- N-ary lambda
slam_NP' :: forall xs r code. (SListI xs, SymFun code) => (NP code xs -> code r) -> code (Curry code r xs)
slam_NP' f = case sList :: SList xs of
    SNil  -> f Nil
    SCons -> lam_ $ \x -> slam_NP' (f . (x :*))

-- N-ary apply
sapply_NP :: forall xs r code. (SListI xs, SymFun code) => code (Curry code r xs) -> NP code xs -> code r
sapply_NP = case sList :: SList xs of
    SNil  -> \r Nil       -> r
    SCons -> \f (x :* xs) -> sapply_NP (app_ f x) xs

-- Elements, acts as tag for sletrec
data Elem :: (k -> Type) -> k -> [[k]] -> k -> Type where
    Here  :: SListI xs         => Elem code r (xs ': xss) (Curry code r xs)
    There :: Elem code r xss f -> Elem code r (ys ': xss) f

instance GEq (Elem expr r xss) where
    geq Here      Here      = Just Refl
    geq (There x) (There y) = geq x y
    geq Here      (There _) = Nothing
    geq (There _) Here      = Nothing

instance GCompare (Elem expr r xss) where
    gcompare Here      Here      = GEQ
    gcompare (There x) (There y) = gcompare x y
    gcompare Here      (There _) = GLT
    gcompare (There _) Here      = GGT

-- Pair of "tycon" and saturated application.
data Pair (code :: k -> Type) (b :: k) (xss :: [[k]]) (xs :: [k]) where
    Pair :: Elem code b xss r
         -> (code r -> NP code xs -> code b)
         -> Pair code b xss xs

pairs :: forall {k} xss (b :: k) code. (SListI2 xss, SymFun code) => NP (Pair code b xss) xss
pairs = case sList :: SList xss of
    SNil  -> Nil
    SCons -> Pair Here sapply_NP :* map_NP shiftPair pairs

shiftPair :: Pair code b xss ys -> Pair code b (xs : xss) ys
shiftPair (Pair el f) = Pair (There el) f

newtype FunTo code b xs = FunTo (NP code xs -> code b)

withNSNP :: (SListI2 xss, SymFun code) => NS (NP code) xss -> (forall r. Elem code b xss r -> (code r -> code b) -> res) -> res
withNSNP (Z np) k = k Here (`sapply_NP` np)
withNSNP (S ns) k = withNSNP ns (\el f -> k (There el) f)
