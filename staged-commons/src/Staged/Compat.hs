{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE UndecidableInstances   #-}
#if __GLASGOW_HASKELL__ >=811
{-# LANGUAGE TemplateHaskellQuotes  #-}
{-# OPTIONS_GHC -Wno-orphans        #-}
#else
{-# LANGUAGE TemplateHaskell        #-}
#endif
module Staged.Compat (
    -- * Code
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
    -- * GHC Splice
    GHCCode,
    IsCode (..),
    fromSplice,
) where

import Data.Kind               (Type)
import Data.String             (IsString (..))
import Language.Haskell.TH     (Q)
import Language.Haskell.TH.Lib (TExpQ)

import Language.Haskell.TH.Syntax.Compat
       (Code (..), CodeQ, IsCode (..), Quote (..), bindCode, bindCode_,
       joinCode, liftCode, unTypeCode, unsafeCodeCoerce)

import qualified Control.Monad.Trans.Class         as Trans
import qualified Language.Haskell.TH.Syntax        as TH
import qualified Language.Haskell.TH.Syntax.Compat as Compat

-- | Short alias for @'Code' 'Q'@
type C = CodeQ

liftTyped :: (TH.Lift a, Quote q) => a -> Code q a
liftTyped = Compat.liftTypedQuote

-------------------------------------------------------------------------------
-- Conversions
-------------------------------------------------------------------------------

-- | Shorter alias for 'examineCode'.
unC :: Code Q a -> TExpQ a
unC = examineCode

transCode :: (Trans.MonadTrans t, Monad q) => Code q a -> Code (t q) a
transCode (Code x) = Code (Trans.lift x)

-------------------------------------------------------------------------------
-- Questionable instances
-------------------------------------------------------------------------------

instance (TH.Lift a, Compat.Quote q, Num a) => Num (Code q a) where
    fromInteger x = liftTyped (fromInteger x)

#if MIN_VERSION_template_haskell(2,17,0)
    x + y = [|| $$x + $$y ||]
    x - y = [|| $$x - $$y ||]
    x * y = [|| $$x * $$y ||]

    abs    x = [|| abs $$x ||]
    negate x = [|| negate $$x ||]
    signum x = [|| signum $$x ||]
#else
    x + y = unsafeCodeCoerce $ fun <$> unTypeCode x <*> unTypeCode y where
        fun x' y' = TH.VarE '(+) `TH.AppE` x' `TH.AppE` y'

    x - y = unsafeCodeCoerce $ fun <$> unTypeCode x <*> unTypeCode y where
        fun x' y' = TH.VarE '(-) `TH.AppE` x' `TH.AppE` y'

    x * y = unsafeCodeCoerce $ fun <$> unTypeCode x <*> unTypeCode y where
        fun x' y' = TH.VarE '(*) `TH.AppE` x' `TH.AppE` y'

    abs x = unsafeCodeCoerce $ fun <$> unTypeCode x where
        fun x' = TH.VarE 'abs `TH.AppE` x'

    negate x = unsafeCodeCoerce $ fun <$> unTypeCode x where
        fun x' = TH.VarE 'negate `TH.AppE` x'

    signum x = unsafeCodeCoerce $ fun <$> unTypeCode x where
        fun x' = TH.VarE 'signum `TH.AppE` x'
#endif

instance (TH.Lift a, Compat.Quote q, IsString a) => IsString (Code q a) where
    fromString  = liftTyped . fromString

-------------------------------------------------------------------------------
-- GHC Splice
-------------------------------------------------------------------------------

-- | A type of GHC typed splices.
-- In some future GHC version it might be 'Code', but not yet.
#if MIN_VERSION_template_haskell(2,17,0)
type GHCCode a = Code Q a
type FromTypedSplice q a = Code q a
#else
type GHCCode a = TExpQ a
type FromTypedSplice (q :: Type -> Type) a = TExpQ a
#endif

fromSplice :: Compat.Quote q => FromTypedSplice q a -> Code q a
#if MIN_VERSION_template_haskell(2,17,0)
fromSplice = id
#else
fromSplice = Code . Compat.unsafeQToQuote
#endif
