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
    Quote, Q,
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
    -- * Because ...
    MonadFix_,
    ) where

import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)
import Language.Haskell.TH (Name, newName, runIO, valD, varP, varE, normalB, letE)
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

sapply :: (IsCode m (a -> b) cab, Quote m) => cab -> Code m a -> Code m b
sapply f x = toCode [|| $$(fromCode $ toCode f) $$(fromCode x) ||]

-- | Infix version of 'sapply'
--
-- /Note:/ the associativity is different from '$'. Thus one can write for exammple:
--
-- @
-- [|| (+) ||] '@@' a '@@' b
-- @
(@@) :: (IsCode m (a -> b) cab, Quote m) => cab -> Code m a -> Code m b
(@@) = sapply

infixl 0 @@

-------------------------------------------------------------------------------
-- Unit
-------------------------------------------------------------------------------

sunit :: Quote m => Code m ()
sunit = toCode [|| () ||]

-------------------------------------------------------------------------------
-- Conditionals
-------------------------------------------------------------------------------

strue :: Quote m => Code m Bool
strue = toCode [|| True ||]

sfalse :: Quote m => Code m Bool
sfalse = toCode [|| False ||]

sIfThenElse :: Quote m => Code m Bool -> Code m a -> Code m a -> Code m a
sIfThenElse b t e = toCode [||
    if $$(fromCode b)
    then $$(fromCode t)
    else $$(fromCode e)
    ||]

------------------------------------------------------------------------------
-- Function conversions
-------------------------------------------------------------------------------

-- | A Class for 'fn' which are morally @Code m a -> Code m b@ functions.
class ToCodeFn m a b fn | fn -> m a b where
    toFn :: fn -> Code m a -> Code m b

instance (fn ~ TExp (a -> b)) => ToCodeFn  Q a b (Q fn) where
    toFn = sapply

instance (fn ~ (a -> b), Quote m) => ToCodeFn m a b (Code m fn) where
    toFn = sapply

instance (a' ~ Code m a, b' ~ Code m b, Quote m) => ToCodeFn m a b (a' -> b') where
    toFn = id

-- | A Class for 'fn' which are morally @Code m a -> Code m b -> Code m c@ functions.
class ToCodeFn2 m a b c fn | fn -> m a b c where
    toFn2 :: fn -> Code m a -> Code m b -> Code m c

instance (fn ~ TExp (a -> b -> c)) => ToCodeFn2 Q a b c (Q fn) where
    toFn2 f x y = f @@ x @@ y

instance (fn ~ (a -> b -> c), Quote m) => ToCodeFn2 m a b c (Code m fn) where
    toFn2 f x y = f @@ x @@ y

instance (a' ~ Code m a, bc' ~ (Code m b -> Code m c), Quote m) => ToCodeFn2 m a b c (a' -> bc') where
    toFn2 = id

fromFn :: Quote m => (Code m a -> Code m b) -> Code m (a -> b)
fromFn f = toCode [|| \_x -> $$(fromCode $ f (toCode [|| _x ||])) ||]

fromFn2 :: Quote m => (Code m a -> Code m b -> Code m c) -> Code m (a -> b -> c)
fromFn2 f = toCode [|| \_x _y -> $$(fromCode $ f (toCode [|| _x ||]) (toCode [|| _y ||])) ||]

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

sid :: Quote m => Code m a -> Code m a
sid = toFn [|| id ||]

sconst :: IsCode m a ca => ca -> Code m b -> Code m a
sconst = const . toCode

-------------------------------------------------------------------------------
-- Let
-------------------------------------------------------------------------------

slet :: Quote q => Code q a -> (Code q a -> Code q r) -> Code q r
slet expr k = toCode [||
        let _x = $$(fromCode expr)
        in $$(fromCode $ k $ toCode [|| _x ||])
    ||]

slet' :: Quote q => Code q a -> (Code q a -> Code q r) -> Code q r
slet' expr k = toCode [||
        let !_x = $$(fromCode expr)
        in $$(fromCode $ k $ toCode [|| _x ||])
    ||]

slam :: Quote q => (Code q a -> Code q b) -> Code q (a -> b)
slam = fromFn

slam' :: Quote q => (Code q a -> Code q b) -> Code q (a -> b)
slam' f = toCode [|| \ !_x -> $$(fromCode $ f (toCode [|| _x ||])) ||]

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
    :: forall a b q. (Ord a, Quote q, MonadFix_ q)
    => (forall m. Monad m => (a -> m (Code q b)) -> (a -> m (Code q b)))
    -> a -> Code q b
sletrec f x = liftCode $ do
    (expr, bindings) <- S.runStateT (loop x) Map.empty
    unsafeTExpCoerce $ letE
        [ valD (varP name) (normalB (unTypeCode code)) []
        | (_, (name, code)) <- Map.toList bindings
        ]
        (unTypeCode expr)
  where
    loop :: a -> M q a b (Code q b)
    loop y = do
        memo <- S.get
        case Map.lookup y memo of
            Nothing -> do
                name <- Trans.lift $ newName $ "_l" ++ show (Map.size memo)
                _ <- mfix_ $ \yCode -> do
                    S.modify (Map.insert y (name, yCode))
                    f loop y
                return $ unsafeCodeCoerce $ varE name

            Just (name, _) ->
                return $ unsafeCodeCoerce $ varE name

type M q a b = S.StateT (Map.Map a (Name, Code q b)) q

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
    :: forall f a q. (Quote q, MonadFix_ q)
    => (forall x y. f x -> f y -> Maybe (x :~: y)) -- ^ equality on equation tags
    -> (forall m b. Monad m => (forall c. f c -> m (Code q c)) -> (f b -> m (Code q b))) -- ^ open recursion callback
    -> f a       -- ^ equation tag
    -> Code q a  -- ^ resulting code
sletrecH eq f x = liftCode $ do
    (expr, bindings) <- S.runStateT (loop x) dmapEmpty
    unsafeTExpCoerce $ letE
        [ valD (varP name) (normalB (unTypeCode code)) []
        | _ :*: NE name code <- dmapToList bindings
        ]
        (unTypeCode expr)
  where
    loop :: forall x. f x -> S.StateT (DMap f (NameExp q)) q (Code q x)
    loop y = do
        memo <- S.get
        case dmapLookup eq y memo of
            Nothing -> do
                name <- Trans.lift $ newName $ "_l" ++ show (dmapSize memo)
                _ <- mfix_ $ \yCode -> do
                    S.modify (dmapInsert y (NE name yCode))
                    f loop y
                return $ unsafeCodeCoerce $ varE name

            Just (NE name _) -> do
                return $ unsafeCodeCoerce $ varE name

data NameExp q b = NE !Name (Code q b)

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

sreturn :: (Quote q, Monad m) => Code q a -> Code q (m a)
sreturn = toFn [|| return ||]

sfmap :: (Quote q, Monad m) => (Code q a -> Code q b) -> Code q (m a) -> Code q (m b)
sfmap f x = toCode [|| fmap ||] @@ fromFn f @@ x

infixl 1 >>>=
(>>>=) :: (Quote q, Monad m) => Code q (m a) -> (Code q a -> Code q (m b)) -> Code q (m b)
m >>>= k = toCode [|| (>>=) ||] @@ m @@ fromFn k

-------------------------------------------------------------------------------
-- Pairs
-------------------------------------------------------------------------------

spair :: Quote m => Code m a -> Code m b -> Code m (a, b)
spair x y = toCode [|| ($$(fromCode x), $$(fromCode y)) ||]

spairElim :: Quote m => Code m (a, b) -> (Code m a -> Code m b -> Code m r) -> Code m r
spairElim p kont = toCode
    [|| case $$(fromCode p) of
            (_pfst, _psnd) -> $$(fromCode $ kont (toCode [|| _pfst ||]) (toCode [|| _psnd ||]))
    ||]

sfst :: Quote m => Code m (a, b) -> Code m a
sfst = toFn [|| fst ||]

ssnd :: Quote m => Code m (a, b) -> Code m b
ssnd = toFn [|| snd ||]

-------------------------------------------------------------------------------
-- Maybe
-------------------------------------------------------------------------------

snothing :: Quote m => Code m (Maybe a)
snothing = toCode [|| Nothing ||]

sjust :: Quote m => Code m a -> Code m (Maybe a)
sjust = toFn [|| Just ||]

smaybe :: Quote m => Code m (Maybe a) -> Code m r -> (Code m a -> Code m r) -> Code m r
smaybe m n j = toCode [|| case $$(fromCode m) of
    Nothing -> $$(fromCode n)
    Just _x -> $$(fromCode $ j $ toCode [|| _x ||])
    ||]

-------------------------------------------------------------------------------
-- Either
-------------------------------------------------------------------------------

sleft :: Quote m => Code m a -> Code m (Either a b)
sleft = toFn [|| Left ||]

sright :: Quote m => Code m b -> Code m (Either a b)
sright = toFn [|| Right ||]

seither :: Quote m => Code m (Either a b) -> (Code m a -> Code m r) -> (Code m b -> Code m r) -> Code m r
seither e l r = toCode [|| case $$(fromCode e) of
    Left _x  -> $$(fromCode $ l $ toCode [|| _x ||])
    Right _y -> $$(fromCode $ r $ toCode [|| _y ||])
    ||]

-------------------------------------------------------------------------------
-- List
-------------------------------------------------------------------------------

snil :: Quote m => Code m [a]
snil = toCode [|| [] ||]

scons :: Quote m => Code m a -> Code m [a] -> Code m [a]
scons x xs = toCode [|| $$(fromCode x) : $$(fromCode xs) ||]

scaseList
    :: Quote m
    => Code m [a]
    -> Code m r
    -> (Code m a -> Code m [a] -> Code m r)
    -> Code m r
scaseList xs nil cons = toCode [||
    case $$(fromCode xs) of
        []       -> $$(fromCode nil)
        (_x:_xs) -> $$(fromCode $ cons (toCode [|| _x ||]) (toCode [|| _xs ||]))
    ||]

-------------------------------------------------------------------------------
-- Num
-------------------------------------------------------------------------------

splus :: (Num a, Quote m) => Code m a -> Code m a -> Code m a
splus = toFn2 [|| (+) ||]

smult :: (Num a, Quote m) => Code m a -> Code m a -> Code m a
smult = toFn2 [|| (*) ||]

-------------------------------------------------------------------------------
-- MonadIO
-------------------------------------------------------------------------------

sliftIO :: (Quote m, MonadIO n) => Code m (IO a) -> Code m (n a)
sliftIO = toFn [|| liftIO ||]

-------------------------------------------------------------------------------
-- Int
-------------------------------------------------------------------------------

-- | @'liftTyped' \@Int@, with type annotation.
sint :: Quote m => Int -> Code m Int
sint n = toCode [|| $$(fromCode $ liftTyped n) :: Int ||]

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
