{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Staged.Commons (
    -- * Types
    Code (..),
    C,
    -- * Conversions
    unC,
    liftCode,
    liftTyped,
    unsafeCodeCoerce,
    unTypeCode,
    joinCode,
    bindCode,
    bindCode_,
    -- * Transformers
    transCode,
    -- * GHC Code
    GHCCode,
    IsCode (..),
    -- * Constructors
    sapply,
    (@@),
    -- * Function conversions
    ToCodeFn (..), fromFn,
    ToCodeFn2 (..), fromFn2,
    -- * Functions
    sid,
    sconst,
    -- * let
    slet, slet',
    slam, slam',
    -- * letrec
    sletrec,
    sletrecH,
    -- * Monad
    sreturn,
    sfmap,
    (>>>=),
    -- * Unit
    sunit,
    -- * Conditionals
    strue,
    sfalse,
    sIfThenElse,
    -- * Pairs
    spair,
    spairElim,
    sfst,
    ssnd,
    -- * Maybe
    snothing,
    sjust,
    smaybe,
    -- * Either
    sleft,
    sright,
    seither,
    -- * Lists
    snil,
    scons,
    scaseList,
    -- * Num
    splus,
    smult,
    -- * MonadIO
    sliftIO,
    -- * Int
    sint,
    ) where

import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)
import Language.Haskell.TH (Name, Q, newName, runIO, valD, varP, varE, normalB, letE)
import Language.Haskell.TH.Syntax (unsafeTExpCoerce, TExp)
import Control.Monad.IO.Class (MonadIO (..))
import System.IO.Unsafe (unsafeInterleaveIO)
import Data.Type.Equality ((:~:) (..))

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State as S
import qualified Data.Map.Lazy as Map

import Staged.Compat

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

sapply :: IsCode (a -> b) cab => cab -> C a -> C b
sapply f x = C [|| $$(unC $ toCode f) $$(unC x) ||]

-- | Infix version of 'sapply'
--
-- /Note:/ the associativity is different from '$'. Thus one can write for exammple:
--
-- @
-- [|| (+) ||] '@@' a '@@' b
-- @
(@@) :: IsCode (a -> b) cab => cab -> C a -> C b
(@@) = sapply

infixl 0 @@

-------------------------------------------------------------------------------
-- Unit
-------------------------------------------------------------------------------

sunit :: C ()
sunit = C [|| () ||]

-------------------------------------------------------------------------------
-- Conditionals
-------------------------------------------------------------------------------

strue :: C Bool
strue = C [|| True ||]

sfalse :: C Bool
sfalse = C [|| False ||]

sIfThenElse :: C Bool -> C a -> C a -> C a
sIfThenElse b t e = C [||
    if $$(unC b)
    then $$(unC t)
    else $$(unC e)
    ||]

------------------------------------------------------------------------------
-- Function conversions
-------------------------------------------------------------------------------

-- | A Class for 'fn' which are morally @Code m a -> Code m b@ functions.
class ToCodeFn a b fn | fn -> a b where
    toFn :: fn -> C a -> C b

instance (fn ~ TExp (a -> b)) => ToCodeFn a b (Q fn) where
    toFn = sapply

instance (fn ~ (a -> b), m ~ Q) => ToCodeFn a b (Code m fn) where
    toFn = sapply

instance (a' ~ Code Q a, b' ~ Code Q b) => ToCodeFn a b (a' -> b') where
    toFn = id

-- | A Class for 'fn' which are morally @Code m a -> Code m b -> Code m c@ functions.
class ToCodeFn2 a b c fn | fn -> a b c where
    toFn2 :: fn -> C a -> C b -> C c

instance (fn ~ TExp (a -> b -> c)) => ToCodeFn2 a b c (Q fn) where
    toFn2 f x y = f @@ x @@ y

instance (fn ~ (a -> b -> c), m ~ Q) => ToCodeFn2 a b c (Code m fn) where
    toFn2 f x y = f @@ x @@ y

instance (a' ~ Code Q a, bc' ~ (Code Q b -> Code Q c)) => ToCodeFn2 a b c (a' -> bc') where
    toFn2 = id

fromFn :: (C a -> C b) -> C (a -> b)
fromFn f = C [|| \_x -> $$(unC $ f (C [|| _x ||])) ||]

fromFn2 :: (C a -> C b -> C c) -> C (a -> b -> c)
fromFn2 f = C [|| \_x _y -> $$(unC $ f (C [|| _x ||]) (C [|| _y ||])) ||]

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

sid :: C a -> C a
sid = toFn [|| id ||]

sconst :: IsCode a ca => ca -> C b -> C a
sconst = const . toCode

-------------------------------------------------------------------------------
-- Let
-------------------------------------------------------------------------------

slet :: C a -> (C a -> C r) -> C r
slet expr k = C [||
        let _x = $$(unC expr)
        in $$(unC $ k $ C [|| _x ||])
    ||]

slet' :: C a -> (C a -> C r) -> C r
slet' expr k = C [||
        let ! _x = $$(unC expr)
        in $$(unC $ k $ C [|| _x ||])
    ||]

slam :: (C a -> C b) -> C (a -> b)
slam = fromFn

slam' :: (C a -> C b) -> C (a -> b)
slam' f = C [|| \ ! _x -> $$(unC $ f (C [|| _x ||])) ||]

-------------------------------------------------------------------------------
-- LetRec
-------------------------------------------------------------------------------

-- | Generating mutually-recursive definitions.
--
-- @
-- fibnr :: Int -> C Int
-- fibnr = 'sletrec' $ \\rec n ->
--     if n == 0 then return ('sint' 1) else
--     if n == 1 then return ('sint' 1) else do
--         n1 <- rec (n - 1)
--         n2 <- rec (n - 2)
--         return $ 'toCode' [|| $$('fromCode' n1) + $$('fromCode' n2) ||]
-- @
--
sletrec
    :: forall a b. Ord a
    => (forall t. (Trans.MonadTrans t, Monad (t Q)) => (a -> t Q (Code Q b)) -> (a -> t Q (Code Q b)))
    -> a -> Code Q b
sletrec f x = liftCode $ do
    (expr, bindings) <- S.runStateT (loop x) Map.empty
    unsafeTExpCoerce $ letE
        [ valD (varP name) (normalB (unTypeCode code)) []
        | (_, (name, code)) <- Map.toList bindings
        ]
        (unTypeCode expr)
  where
    loop :: a -> M a b (Code Q b)
    loop y = do
        memo <- S.get
        case Map.lookup y memo of
            Nothing -> do
                name <- Trans.lift $ newName $ "_l" ++ show (Map.size memo)
                _ <- mfix_ $ \yCode -> do
                    S.modify (Map.insert y (name, yCode))
                    f loop y
                return $ C $ unsafeTExpCoerce $ varE name

            Just (name, _) ->
                return $ C $ unsafeTExpCoerce $ varE name

type M a b = S.StateT (Map.Map a (Name, Code Q b)) Q

-------------------------------------------------------------------------------
-- Heteregeneous LetRec
-------------------------------------------------------------------------------

-- | Generating heterogenous mutually-recursive definitions.
--
-- The setup is somewhat involved. Suppose we have two mutually-recursive
-- data types:
--
-- @
-- data Even = Zero | E1 Odd deriving Show
-- data Odd  = O1 Even       deriving Show
-- @
--
-- and we want to generate function of type @Int -> Maybe Even@.
-- We will generate two mutually recursive functions, @isEven@ and @isOdd@.
-- First we need tags telling the type of symbols we want to generate.
--
-- @
-- data Tag a where
--     TagE :: Tag (Int -> Maybe Even)
--     TagO :: Tag (Int -> Maybe Odd)
-- @
--
-- The tag could have more fields. It all boils down to implementing
-- an equality predicate. In this case it is quite straight-forward:
--
-- @
-- eqTag :: Tag a -> Tag b -> Maybe (a :~: b)
-- eqTag TagE TagE = Just Refl
-- eqTag TagE TagO = Nothing
-- eqTag TagO TagE = Nothing
-- eqTag TagO TagO = Just Refl
-- @
--
-- Then we can write generating code. I apologize for this contrived example.
--
-- @
-- isEvenCodeH :: Tag sig -> C sig
-- isEvenCodeH = 'sletrecH' eqTag $ \rec tag -> case tag of
--     TagE -> do
--         isOdd <- rec TagO
--         return $ 'toCode' [|| \\n -> case n of
--                                 0 -> Just Zero
--                                 _ -> E1 \<$> $$('fromCode' isOdd) (n - 1)
--                          ||]
--
--     TagO -> do
--         isEven <- rec TagE
--         return $ 'toCode' [|| \n -> case n of
--                                 0 -> Nothing
--                                 _ -> O1 \<$> $$('fromCode' isEven) (n - 1)
--                          ||]
-- @
--
-- The generated code is what we would expect:
--
-- @
--     unC isEvenCodeH
--   ======>
--     let
--       _l1_aaxb
--         = \ n_aaxc
--             -> case n_aaxc of
--                  0 -> Nothing
--                  _ -> (O1 \<$> _l0_aaxa (n_aaxc - 1))
--       _l0_aaxa
--         = \ n_aaxd
--             -> case n_aaxd of
--                  0 -> Just Zero
--                  _ -> (E1 \<$> _l1_aaxb (n_aaxd - 1))
--     in _l0_aaxa
-- @
--
-- See also @some@ library for starting point for heterogenous utilities,
-- e.g. @GEq@ type-class for equality.
--
sletrecH
    :: forall f a. (forall x y. f x -> f y -> Maybe (x :~: y)) -- ^ equality on equation tags
    -> (forall t b. (Trans.MonadTrans t, Monad (t Q)) => (forall c. f c -> t Q (Code Q c)) -> (f b -> t Q (Code Q b))) -- ^ open recursion callback
    -> f a   -- ^ equation tag
    -> Code Q a  -- ^ resulting code
sletrecH eq f x = liftCode $ do
    (expr, bindings) <- S.runStateT (loop x) dmapEmpty
    unsafeTExpCoerce $ letE
        [ valD (varP name) (normalB (unTypeCode code)) []
        | _ :*: NE name code <- dmapToList bindings
        ]
        (unTypeCode expr)
  where
    loop :: forall x. f x -> S.StateT (DMap f NameExp) Q (Code Q x)
    loop y = do
        memo <- S.get
        case dmapLookup eq y memo of
            Nothing -> do
                name <- Trans.lift $ newName $ "_l" ++ show (dmapSize memo)
                _ <- mfix_ $ \yCode -> do
                    S.modify (dmapInsert y (NE name yCode))
                    f loop y
                return $ C $ unsafeTExpCoerce $ varE name

            Just (NE name _) -> do
                return $ C $ unsafeTExpCoerce $ varE name

data NameExp b = NE !Name (Code Q b)

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

sreturn :: Monad m => C a -> C (m a)
sreturn = toFn [|| return ||]

sfmap :: Monad m => (C a -> C b) -> C (m a) -> C (m b)
sfmap f x = C [|| fmap ||] @@ fromFn f @@ x

infixl 1 >>>=
(>>>=) :: Monad m => C (m a) -> (C a -> C (m b)) -> C (m b)
m >>>= k = C [|| (>>=) ||] @@ m @@ fromFn k

-------------------------------------------------------------------------------
-- Pairs
-------------------------------------------------------------------------------

spair :: C a -> C b -> C (a, b)
spair x y = C [|| ($$(unC x), $$(unC y)) ||]

spairElim :: C (a, b) -> (C a -> C b -> C r) -> C r
spairElim (C p) kont = C
    [|| case $$p of
            (_pfst, _psnd) -> $$(unC $ kont (C [|| _pfst ||]) (C [|| _psnd ||]))
    ||]

sfst :: C (a, b) -> C a
sfst = toFn [|| fst ||]

ssnd :: C (a, b) -> C b
ssnd = toFn [|| snd ||]

-------------------------------------------------------------------------------
-- Maybe
-------------------------------------------------------------------------------

snothing :: C (Maybe a)
snothing = C [|| Nothing ||]

sjust :: C a -> C (Maybe a)
sjust = toFn [|| Just ||]

smaybe :: C (Maybe a) -> C r -> (C a -> C r) -> C r
smaybe m n j = C [|| case $$(unC m) of
    Nothing -> $$(unC n)
    Just _x -> $$(unC $ j $ C [|| _x ||])
    ||]

-------------------------------------------------------------------------------
-- Either
-------------------------------------------------------------------------------

sleft :: C a -> C (Either a b)
sleft = toFn [|| Left ||]

sright :: C b -> C (Either a b)
sright = toFn [|| Right ||]

seither :: C (Either a b) -> (C a -> C r) -> (C b -> C r) -> C r
seither e l r = C [|| case $$(unC e) of
    Left _x  -> $$(unC $ l $ C [|| _x ||])
    Right _y -> $$(unC $ r $ C [|| _y ||])
    ||]

-------------------------------------------------------------------------------
-- List
-------------------------------------------------------------------------------

snil :: C [a]
snil = C [|| [] ||]

scons :: C a -> C [a] -> C [a]
scons x xs = C [|| $$(unC x) : $$(unC xs) ||]

scaseList
    :: C [a]
    -> C r
    -> (C a -> C [a] -> C r)
    -> C r
scaseList xs nil cons = C [||
    case $$(unC xs) of
        []       -> $$(unC nil)
        (_x:_xs) -> $$(unC $ cons (C [|| _x ||]) (C [|| _xs ||]))
    ||]

-------------------------------------------------------------------------------
-- Num
-------------------------------------------------------------------------------

splus :: Num a => C a -> C a -> C a
splus = toFn2 [|| (+) ||]

smult :: Num a => C a -> C a -> C a
smult = toFn2 [|| (*) ||]

-------------------------------------------------------------------------------
-- MonadIO
-------------------------------------------------------------------------------

sliftIO :: MonadIO m => C (IO a) -> C (m a)
sliftIO = toFn [|| liftIO ||]

-------------------------------------------------------------------------------
-- Int
-------------------------------------------------------------------------------

-- | @'liftTyped' \@Int@, with type annotation.
sint :: Int -> C Int
sint n = C [|| $$(unC $ liftTyped n) :: Int ||]

-------------------------------------------------------------------------------
-- Our version of MonadFix, to avoid orphan instance
-------------------------------------------------------------------------------

class MonadFix_ m where
    mfix_ :: (a -> m a) -> m a

instance MonadFix_ m => MonadFix_ (S.StateT s m) where
    mfix_ f = S.StateT $ \ s -> mfix_ $ \ ~(a, _) -> S.runStateT (f a) s
    {-# INLINE mfix_ #-}

instance MonadFix_ Q where
    mfix_ k = do
        m <- runIO newEmptyMVar
        ans <- runIO (unsafeInterleaveIO (takeMVar m))
        result <- k ans
        runIO (putMVar m result)
        pure result
    {-# INLINE mfix_ #-}

-------------------------------------------------------------------------------
-- DSum and DMap, kind of
-------------------------------------------------------------------------------

data DSum f g where
    (:*:) :: !(f a) -> g a -> DSum f g

newtype DMap f g = DMap { dmapToList :: [DSum f g] }

dmapEmpty :: DMap f g
dmapEmpty = DMap []

dmapLookup
    :: (forall x y. f x -> f y -> Maybe (x :~: y))
    -> f a
    -> DMap f g
    -> Maybe (g a)
dmapLookup _eq _k (DMap [])               = Nothing
dmapLookup  eq  k (DMap ((x :*: y) : zs)) = case eq k x of
    Just Refl -> Just y
    Nothing   -> dmapLookup eq k (DMap zs)

dmapSize :: DMap f g -> Int
dmapSize (DMap xs) = length xs

dmapInsert :: f a -> g a -> DMap f g -> DMap f g
dmapInsert k v (DMap xs) = DMap ((k :*: v) : xs)
