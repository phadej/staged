{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
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
    -- * let (TODO)

    -- * letrec
    Fixedpoint,
    sletrec_SOP,
    sletrec1_SOP,
    sletrec_NSNP,
    sletrec1_NSNP,
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

{-

    -- * Mapping helpers
    -- ** Unary
    mapIC,
    mapKC,
    mapCI,
    mapCK,
    mapCC,
    -- ** Binary
    mapIIC,
    mapIKC,
    mapICI,
    mapICK,
    mapICC,
    mapKIC,
    mapKKC,
    mapKCI,
    mapKCK,
    mapKCC,
    mapCII,
    mapCIK,
    mapCIC,
    mapCKI,
    mapCKK,
    mapCKC,
    mapCCI,
    mapCCK,
    mapCCC,

-}
    ) where

import Language.Haskell.TH (Name, DecQ, PatQ, ExpQ, Q, newName, varP, bangP, varE, funD, clause, normalB, letE, appE)
import Language.Haskell.TH.Lib (TExpQ)
import Language.Haskell.TH.Syntax (unTypeQ, unsafeTExpCoerce, TExp)
import Data.List (foldl')
import Control.Monad.IO.Class (MonadIO (..))
import Data.SOP
import Data.SOP.NP
import Data.SOP.NS

import qualified Control.Monad.Trans.State as S

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
-- generics-sop inspired functions
-------------------------------------------------------------------------------

{-

-- arity: 2

mapIC :: (x -> C x) -> I x -> C x
mapIC f (I a) = C (f a)

mapKC :: (a -> C x) -> K a x -> C x
mapKC f (K a) = C (f a)

mapCI :: (C x -> x) -> C x -> I x
mapCI f (C a) = I (f a)

mapCK :: (C x -> a) -> C x -> K a x
mapCK f (C a) = K (f a)

mapCC :: (C x -> C x) -> C x -> C x
mapCC f (C a) = C (f a)

-- arity: 3

mapIIC :: ( x ->  x -> C x) -> I x -> I x -> C x
mapIIC f (I a) (I b) = C (f a b)

mapIKC :: (x -> a -> C x) -> I x -> K a x -> C x
mapIKC f (I a) (K b) = C (f a b)

mapICI :: (x -> C x -> x) -> I x -> C x -> I x
mapICI f (I a) (C b) = I (f a b)

mapICK :: (x -> C x -> a) -> I x -> C x -> K a x
mapICK f (I a) (C b) = K (f a b)

mapICC :: (x -> C x -> C x) -> I x -> C x -> C x
mapICC f (I a) (C b) = C (f a b)

mapKIC :: (a -> x -> C x) -> K a x -> I x -> C x
mapKIC f (K a) (I b) = C (f a b)

mapKKC :: (a -> b -> C x) -> K a x -> K b x -> C x
mapKKC f (K a) (K b) = C (f a b)

mapKCI :: (a -> C x -> x) -> K a x -> C x -> I x
mapKCI f (K a) (C b) = I (f a b)

mapKCK :: (a -> C x -> b) -> K a x -> C x -> K b x
mapKCK f (K a) (C b) = K (f a b)

mapKCC :: (a -> C x -> C x) -> K a x -> C x -> C x
mapKCC f (K a) (C b) = C (f a b)

mapCII :: (C x -> x -> x) -> C x -> I x -> I x
mapCII f (C a) (I b) = I (f a b)

mapCIK :: (C x ->  x -> a) -> C x -> I x -> K a x
mapCIK f (C a) (I b) = K (f a b)

mapCIC :: (C x -> x -> C x) -> C x -> I x -> C x
mapCIC f (C a) (I b) = C (f a b)

mapCKI :: (C x -> a -> x) -> C x -> K a x -> I x
mapCKI f (C a) (K b) = I (f a b)

mapCKK :: (C x -> a -> b) -> C x -> K a x -> K b x
mapCKK f (C a) (K b) = K (f a b)

mapCKC :: (C x -> a -> C x) -> C x -> K a x -> C x
mapCKC f (C a) (K b) = C (f a b)

mapCCI :: (C x -> C x ->  x) -> C x -> C x -> I x
mapCCI f (C a) (C b) = I (f a b)

mapCCK :: (C x -> C x -> a) -> C x -> C x -> K a x
mapCCK f (C a) (C b) = K (f a b)

mapCCC :: (C x -> C x -> C x) -> C x -> C x -> C x
mapCCC f (C a) (C b) = C (f a b)

-}

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
-- LetRec
-------------------------------------------------------------------------------

-- | Type of 'fix'. Fixedpoint of @a@.
type Fixedpoint a = (a -> a) -> a

sletrec_SOP
    :: forall xss b. SListI2 xss => Fixedpoint (SOP C xss -> C b)
sletrec_SOP f x = sletrec_NSNP (\y z -> f (y . unSOP) (SOP z)) (unSOP x)

sletrec_NSNP
    :: forall xss b. SListI2 xss => Fixedpoint (NS (NP C) xss -> C b)
sletrec_NSNP body args0 = liftCode $ do
    states <- S.evalStateT (sequence'_NP $ pure_NP $ Comp $ K <$> newNameIndex "_state") (0 :: Int)
    anames <- S.evalStateT (fmap unPOP $ sequence'_POP $ pure_POP $ Comp $ K <$> newNameIndex "_x") (0 :: Int)
    unsafeTExpCoerce $ letE
        (collapse_NP (cliftA3_NP (Proxy :: Proxy SListI)  (mkFun states) injections' states anames :: NP (K DecQ) xss))
        (unTypeQ $ call states args0)
  where
    body' :: (NS (NP C) xss -> TExpQ b)
          ->  NS (NP C) xss -> TExpQ b
    body' rec = unC . body (liftCode . rec)

    mkFun :: SListI xs => NP (K Name) xss -> Injection (NP C) xss xs -> K Name xs -> NP (K Name) xs -> K DecQ xs
    mkFun states (Fn inj) (K state) as =
        K $ funD state [ clause (varsP as) (normalB $ unTypeQ $ body' (call states) (unK $ inj $ varsE as)) []]

    injections' :: NP (Injection (NP C) xss) xss
    injections' = injections

    call :: SListI2 xss => NP (K Name) xss -> NS (NP C) xss -> TExpQ c
    call states args = collapse_NS $ cliftA2_NS (Proxy :: Proxy SListI)
        (\(K state) args' -> K $ unsafeTExpCoerce $ appsE (varE state) (mkArgs args'))
        states args

-- | 'sletrec_SOP' with additional argument in each state.
sletrec1_SOP
    :: forall xss b c. SListI2 xss => Fixedpoint (SOP C xss -> C b -> C c)
sletrec1_SOP f x = sletrec1_NSNP (\u v w -> f (u . unSOP) (SOP v) w) (unSOP x)

-- | 'sletrec_NSNP' with additional argument in each state.
sletrec1_NSNP
    :: forall xss b c. SListI2 xss => Fixedpoint (NS (NP C) xss -> C b -> C c)
sletrec1_NSNP body args0 b0 = liftCode $ do
    states <- S.evalStateT (sequence'_NP $ pure_NP $ Comp $ K <$> newNameIndex "_state") (0 :: Int)
    anames <- S.evalStateT (fmap unPOP $ sequence'_POP $ pure_POP $ Comp $ K <$> newNameIndex "_x") (0 :: Int)
    bname  <- newName "_b"
    unsafeTExpCoerce $ letE
        (collapse_NP (cliftA3_NP (Proxy :: Proxy SListI)  (mkFun states bname) injections' states anames :: NP (K DecQ) xss))
        (unTypeQ $ call states args0 (unC b0))
  where
    body' :: (NS (NP C) xss -> TExpQ b -> TExpQ c)
          ->  NS (NP C) xss -> TExpQ b -> TExpQ c
    body' rec x y = unC $
        body (\x' y' -> liftCode (rec x' (unC y'))) x (liftCode y)

    mkFun :: SListI xs => NP (K Name) xss -> Name -> Injection (NP C) xss xs -> K Name xs -> NP (K Name) xs -> K DecQ xs
    mkFun states b (Fn inj) (K state) as =
        K $ funD state [ clause (bangP (varP b) : varsP as) (normalB $ unTypeQ $ body' (call states) (unK $ inj $ varsE as) b') []]
      where
        b' :: TExpQ b
        b' = unsafeTExpCoerce (varE b)

    injections' :: NP (Injection (NP C) xss) xss
    injections' = injections

    call :: SListI2 xss => NP (K Name) xss -> NS (NP C) xss -> TExpQ b -> TExpQ c
    call states args b = collapse_NS $ cliftA2_NS (Proxy :: Proxy SListI)
        (\(K state) args' -> K $ unsafeTExpCoerce $ appsE (varE state) (unTypeQ b : mkArgs args'))
        states args

appsE :: ExpQ -> [ExpQ] -> ExpQ
appsE = foldl' appE

varsP :: SListI xs => NP (K Name) xs -> [PatQ]
varsP names = collapse_NP $ map_NP (mapKK (bangP . varP)) names

varsE :: SListI xs => NP (K Name) xs -> NP C xs
varsE names = map_NP (\(K n) -> C (unsafeTExpCoerce (varE n))) names

mkArgs ::SListI xs => NP C xs -> [ExpQ]
mkArgs args = collapse_NP $ map_NP (\(C x) -> K (unTypeQ x)) args

newNameIndex :: String -> S.StateT Int Q Name
newNameIndex pfx = S.StateT $ \i -> do
    n <- newName (pfx ++ show i)
    return (n, i + 1)

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
