{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
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

import Control.Concurrent.MVar    (newEmptyMVar, putMVar, takeMVar)
import Control.Monad.IO.Class     (MonadIO (..))
import Data.Type.Equality         ((:~:) (..))
import Language.Haskell.TH        (Name, runIO)
import Language.Haskell.TH.Syntax (TExp)
import System.IO.Unsafe           (unsafeInterleaveIO)

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State as S
import qualified Data.Kind                 as Kind
import qualified Data.Map.Lazy             as Map

import qualified Language.Haskell.TH.Syntax        as TH
import qualified Language.Haskell.TH.Syntax.Compat as Compat

import Staged.Compat

#if __GLASGOW_HASKELL__ >= 811
#define HAS_CODE_SPLICE 1
#else
#define HAS_CODE_SPLICE 0
#endif

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

sapply :: (IsCode m (a -> b) cab, Quote m) => cab -> Code m a -> Code m b
#if HAS_CODE_SPLICE
sapply f x = [|| $$(toCode f) $$x ||]
#else
sapply f x =
    unsafeCodeCoerce $ aux <$> unTypeCode (toCode f) <*> unTypeCode x
  where
    aux = TH.AppE
#endif

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
sunit = fromSplice [|| () ||]

-------------------------------------------------------------------------------
-- Conditionals
-------------------------------------------------------------------------------

strue :: Quote m => Code m Bool
strue = fromSplice [|| True ||]

sfalse :: Quote m => Code m Bool
sfalse = fromSplice [|| False ||]

sIfThenElse :: Quote m => Code m Bool -> Code m a -> Code m a -> Code m a
#if HAS_CODE_SPLICE
sIfThenElse b t e = toCode [|| if $$b then $$t else $$e ||]
#else
sIfThenElse b t e =
    unsafeCodeCoerce $ aux <$> unTypeCode b <*> unTypeCode t <*> unTypeCode e
  where
    aux = TH.CondE
#endif

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
#if HAS_CODE_SPLICE
fromFn f = [|| \_x -> $$(f [|| _x ||]) ||]
#else
fromFn f = unsafeCodeCoerce $ do
    x <- Compat.newName "_x"
    body' <- unTypeCode (f (nameCode x))
    return $ TH.LamE [TH.VarP x] body'
#endif

fromFn2 :: Quote m => (Code m a -> Code m b -> Code m c) -> Code m (a -> b -> c)
#if HAS_CODE_SPLICE
fromFn2 f = [|| \_x _y -> $$(f [|| _x ||] [|| _y ||]) ||]
#else
fromFn2 f = unsafeCodeCoerce $ do
    x <- Compat.newName "_x"
    y <- Compat.newName "_y"
    body' <- unTypeCode (f (nameCode x) (nameCode y))
    return $ TH.LamE [TH.VarP x, TH.VarP y] body'
#endif

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

sid :: Quote m => Code m a -> Code m a
sid = id

sconst :: IsCode m a ca => ca -> Code m b -> Code m a
sconst = const . toCode

-------------------------------------------------------------------------------
-- Let
-------------------------------------------------------------------------------

slet :: Quote q => Code q a -> (Code q a -> Code q r) -> Code q r
#if HAS_CODE_SPLICE
slet expr k = [|| let _x = $$expr in $$(k [|| _x ||]) ||]
#else
slet expr k = unsafeCodeCoerce $ do
    x <- Compat.newName "_x"
    expr' <- unTypeCode expr
    body' <- unTypeCode (k (nameCode x))
    return $ TH.LetE [ TH.ValD (TH.VarP x) (TH.NormalB expr') []] body'
#endif

slet' :: Quote q => Code q a -> (Code q a -> Code q r) -> Code q r
#if HAS_CODE_SPLICE
slet' expr k = [|| let !_x = $$expr in $$(k [|| _x ||]) ||]
#else
slet' expr k = unsafeCodeCoerce $ do
    x <- Compat.newName "_x"
    expr' <- unTypeCode expr
    body' <- unTypeCode (k (nameCode x))
    return $ TH.LetE [ TH.ValD (TH.BangP (TH.VarP x)) (TH.NormalB expr') []] body'
#endif

slam :: Quote q => (Code q a -> Code q b) -> Code q (a -> b)
slam = fromFn

slam' :: Quote q => (Code q a -> Code q b) -> Code q (a -> b)
#if HAS_CODE_SPLICE
slam' f = [|| \ !_x -> $$(f [|| _x ||]) ||]
#else
slam' f = unsafeCodeCoerce $ do
    x <- Compat.newName "_x"
    body' <- unTypeCode (f (nameCode x))
    return $ TH.LamE [TH.BangP (TH.VarP x)] body'
#endif

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
    mkLet bindings expr
  where
    mkLet :: Map.Map a (Name, Code q b) -> Code q b -> q (TExp b)
    mkLet bindings expr = do
        expr'     <- unTypeCode expr
        bindings' <- (traverse . traverse) unTypeCode bindings
        return $ TH.TExp $ TH.LetE
            [ TH.ValD (TH.VarP name) (TH.NormalB code) []
            | (_, (name, code)) <- Map.toList bindings'
            ]
            expr'

    loop :: a -> M q a b (Code q b)
    loop y = do
        memo <- S.get
        case Map.lookup y memo of
            Nothing -> do
                name <- Trans.lift $ Compat.newName $ "_l" ++ show (Map.size memo)
                _ <- mfix_ $ \yCode -> do
                    S.modify (Map.insert y (name, yCode))
                    f loop y
                return $ nameCode name

            Just (name, _) ->
                return $ nameCode name

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
    :: forall f (a :: Kind.Type) q. (Quote q, MonadFix_ q)
    => (forall x y. f x -> f y -> Maybe (x :~: y)) -- ^ equality on equation tags
    -> (forall m (b :: Kind.Type). Monad m => (forall c. f c -> m (Code q c)) -> (f b -> m (Code q b))) -- ^ open recursion callback
    -> f a       -- ^ equation tag
    -> Code q a  -- ^ resulting code
sletrecH eq f x = liftCode $ do
    (expr, bindings) <- S.runStateT (loop x) dmapEmpty
    mkLet bindings expr
  where
    mkLet :: DMap f (NameExp q) -> Code q a -> q (TExp a)
    mkLet bindings expr = do
        expr' <- unTypeCode expr
        bindings' <- traverse sequence [ (name, unTypeCode code) | _ :*: NE name code <- dmapToList bindings ]
        return $ TH.TExp $ TH.LetE
            [ TH.ValD (TH.VarP name) (TH.NormalB code) []
            | (name, code) <- bindings'
            ]
            expr'

    loop :: forall x. f x -> S.StateT (DMap f (NameExp q)) q (Code q x)
    loop y = do
        memo <- S.get
        case dmapLookup eq y memo of
            Nothing -> do
                name <- Trans.lift $ Compat.newName $ "_l" ++ show (dmapSize memo)
                _ <- mfix_ $ \yCode -> do
                    S.modify (dmapInsert y (NE name yCode))
                    f loop y
                return $ nameCode name

            Just (NE name _) -> do
                return $ nameCode name

data NameExp q b = NE !Name (Code q b)

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

sreturn :: (Quote q, Monad m) => Code q a -> Code q (m a)
#if HAS_CODE_SPLICE
sreturn = toFn [|| return ||]
#else
sreturn = toFnNameVar 'return
#endif

sfmap :: (Quote q, Monad m) => (Code q a -> Code q b) -> Code q (m a) -> Code q (m b)
#if HAS_CODE_SPLICE
sfmap f x = toCode [|| fmap ||] @@ fromFn f @@ x
#else
sfmap f x = unsafeCodeCoerce $ do
    x' <- unTypeCode x
    y <- Compat.newName "_y"
    f' <- unTypeCode $ f (nameCode y)
    return $ TH.VarE 'fmap `TH.AppE` TH.LamE [TH.VarP y] f' `TH.AppE` x'

#endif

infixl 1 >>>=
(>>>=) :: (Quote q, Monad m) => Code q (m a) -> (Code q a -> Code q (m b)) -> Code q (m b)
#if HAS_CODE_SPLICE
m >>>= k = toCode [|| (>>=) ||] @@ m @@ fromFn k
#else
m >>>= k = unsafeCodeCoerce $ do
    m' <- unTypeCode m
    y <- Compat.newName "_y"
    k' <- unTypeCode $ k (nameCode y)
    return $ TH.VarE '(>>=) `TH.AppE` m' `TH.AppE` TH.LamE [TH.VarP y] k'
#endif

-------------------------------------------------------------------------------
-- Pairs
-------------------------------------------------------------------------------

spair :: Quote m => Code m a -> Code m b -> Code m (a, b)
#if HAS_CODE_SPLICE
spair x y = [|| ($$x, $$y) ||]
#else
spair x y = unsafeCodeCoerce $ aux <$> unTypeCode x <*> unTypeCode y where
#if MIN_VERSION_template_haskell(2,16,0)
    aux x' y' = TH.TupE [Just x', Just y']
#else
    aux x' y' = TH.TupE [x', y']
#endif
#endif

spairElim :: Quote m => Code m (a, b) -> (Code m a -> Code m b -> Code m r) -> Code m r
#if HAS_CODE_SPLICE
spairElim p kont = toCode
    [|| case $$p of
            (_pfst, _psnd) -> $$(kont [|| _pfst ||] [|| _psnd ||])
    ||]
#else
spairElim p kont = unsafeCodeCoerce $ do
    p' <- unTypeCode p
    pfst <- Compat.newName "_pfst"
    psnd <- Compat.newName "_psnd"
    body' <- unTypeCode (kont (nameCode pfst) (nameCode psnd))
    return $ TH.CaseE p'
        [ TH.Match (TH.TupP [TH.VarP pfst, TH.VarP psnd]) (TH.NormalB body') []
        ]
#endif

sfst :: Quote m => Code m (a, b) -> Code m a
#if HAS_CODE_SPLICE
sfst = toFn [|| fst ||]
#else
sfst = toFnNameVar 'fst
#endif

ssnd :: Quote m => Code m (a, b) -> Code m b
#if HAS_CODE_SPLICE
ssnd = toFn [|| snd ||]
#else
ssnd = toFnNameVar 'snd
#endif

-------------------------------------------------------------------------------
-- Maybe
-------------------------------------------------------------------------------

snothing :: Quote m => Code m (Maybe a)
snothing = fromSplice [|| Nothing ||]

sjust :: Quote m => Code m a -> Code m (Maybe a)
#if HAS_CODE_SPLICE
sjust = toFn [|| Just ||]
#else
sjust = toFnNameCon 'Just
#endif

smaybe :: Quote m => Code m (Maybe a) -> Code m r -> (Code m a -> Code m r) -> Code m r
#if HAS_CODE_SPLICE
smaybe m n j = [|| case $$m of
    Nothing -> $$n
    Just _x -> $$(j [|| _x ||])
    ||]
#else
smaybe m n j = unsafeCodeCoerce $ do
    x <- Compat.newName "_x"
    m' <- unTypeCode m
    n' <- unTypeCode n
    j' <- unTypeCode $ j (nameCode x)
    return $ TH.CaseE m'
        [ TH.Match (TH.ConP 'Nothing [])          (TH.NormalB n') []
        , TH.Match (TH.ConP 'Just    [TH.VarP x]) (TH.NormalB j') []
        ]
#endif

-------------------------------------------------------------------------------
-- Either
-------------------------------------------------------------------------------

sleft :: Quote m => Code m a -> Code m (Either a b)
#if HAS_CODE_SPLICE
sleft = toFn [|| Left ||]
#else
sleft = toFnNameCon 'Left
#endif

sright :: Quote m => Code m b -> Code m (Either a b)
#if HAS_CODE_SPLICE
sright = toFn [|| Right ||]
#else
sright = toFnNameCon 'Right
#endif

seither :: Quote m => Code m (Either a b) -> (Code m a -> Code m r) -> (Code m b -> Code m r) -> Code m r
#if HAS_CODE_SPLICE
seither e l r = [|| case $$e of
    Left _x  -> $$(l $ [|| _x ||])
    Right _y -> $$(r $ [|| _y ||])
    ||]
#else
seither e l r = unsafeCodeCoerce $ do
    x <- Compat.newName "_x"
    y <- Compat.newName "_y"
    e' <- unTypeCode e
    l' <- unTypeCode $ l (nameCode x)
    r' <- unTypeCode $ r (nameCode y)
    return $ TH.CaseE e'
        [ TH.Match (TH.ConP 'Left  [TH.VarP x]) (TH.NormalB l') []
        , TH.Match (TH.ConP 'Right [TH.VarP y]) (TH.NormalB r') []
        ]
#endif

-------------------------------------------------------------------------------
-- List
-------------------------------------------------------------------------------

snil :: Quote m => Code m [a]
snil = fromSplice [|| [] ||]

scons :: Quote m => Code m a -> Code m [a] -> Code m [a]
#if HAS_CODE_SPLICE
scons x xs = [|| $$x : $$xs ||]
#else
scons x xs = unsafeCodeCoerce $ aux <$> unTypeCode x <*> unTypeCode xs where
    aux x' xs' = TH.InfixE (Just x') (TH.ConE '(:)) (Just xs')
#endif

scaseList
    :: Quote m
    => Code m [a]
    -> Code m r
    -> (Code m a -> Code m [a] -> Code m r)
    -> Code m r
#if HAS_CODE_SPLICE
scaseList xs nil cons = [||
    case $$xs of
        []       -> $$nil
        (_x:_xs) -> $$(cons [|| _x ||] [|| _xs ||])
    ||]
#else
scaseList xs nil cons = unsafeCodeCoerce $ do
    y  <- Compat.newName "_y"
    ys <- Compat.newName "_ys"
    xs' <- unTypeCode xs
    nil' <- unTypeCode nil
    cons' <- unTypeCode $ cons (nameCode y) (nameCode ys)
    return $ TH.CaseE xs'
        [ TH.Match (TH.ListP [])                          (TH.NormalB nil')  []
        , TH.Match (TH.ConP '(:) [TH.VarP y, TH.VarP ys]) (TH.NormalB cons') []
        ]
#endif

-------------------------------------------------------------------------------
-- Num
-------------------------------------------------------------------------------

splus :: (Num a, Quote m) => Code m a -> Code m a -> Code m a
#if HAS_CODE_SPLICE
splus = toFn2 [|| (+) ||]
#else
splus = toFnNameVar2 '(+)
#endif

smult :: (Num a, Quote m) => Code m a -> Code m a -> Code m a
#if HAS_CODE_SPLICE
smult = toFn2 [|| (*) ||]
#else
smult = toFnNameVar2 '(*)
#endif

-------------------------------------------------------------------------------
-- MonadIO
-------------------------------------------------------------------------------

sliftIO :: (Quote m, MonadIO n) => Code m (IO a) -> Code m (n a)
#if HAS_CODE_SPLICE
sliftIO = toFn [|| liftIO ||]
#else
sliftIO = toFnNameVar 'liftIO
#endif

-------------------------------------------------------------------------------
-- Int
-------------------------------------------------------------------------------

-- | @'liftTyped' \@Int@, with type annotation.
sint :: Quote m => Int -> Code m Int
#if HAS_CODE_SPLICE
sint n = toCode [|| $$(fromCode $ liftTyped n) :: Int ||]
#else
sint n = unsafeCodeCoerce $ return $ TH.SigE (TH.LitE (TH.IntegerL (toInteger n))) (TH.ConT ''Int)
#endif

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

-------------------------------------------------------------------------------
-- Helpers for defining functions
-------------------------------------------------------------------------------

nameCode :: Quote q => Name -> Code q a
nameCode = unsafeCodeCoerce . return . TH.VarE

#if HAS_CODE_SPLICE
#else
toFnNameVar :: Quote q => Name -> Code q a -> Code q b
toFnNameVar n x = unsafeCodeCoerce $ aux <$> unTypeCode x where
    aux = TH.AppE (TH.VarE n)

toFnNameVar2 :: Quote q => Name -> Code q a -> Code q b -> Code q c
toFnNameVar2 n x y = unsafeCodeCoerce $ aux <$> unTypeCode x <*> unTypeCode y where
    aux x' y' = TH.VarE n `TH.AppE` x' `TH.AppE` y'

toFnNameCon :: Quote q => Name -> Code q a -> Code q b
toFnNameCon n x = unsafeCodeCoerce $ aux <$> unTypeCode x where
    aux = TH.AppE (TH.ConE n)
#endif
