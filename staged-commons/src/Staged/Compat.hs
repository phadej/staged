{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE GADTs                  #-}
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
) where

import Data.String                (IsString (..))
import Language.Haskell.TH        (Q)
import Language.Haskell.TH.Lib    (TExpQ)

#if MIN_VERSION_template_haskell(2,17,0)
import Language.Haskell.TH        (Code (..), CodeQ)
import Language.Haskell.TH.Syntax
       (bindCode, bindCode_, joinCode, liftCode, liftTypedQuote, unTypeCode,
       unsafeCodeCoerce, Quote)
#else
import Language.Haskell.TH.Syntax (unsafeTExpCoerce)
#endif

import qualified Control.Monad.Trans.Class  as Trans
import qualified Language.Haskell.TH.Syntax as TH

import Language.Haskell.TH.Syntax.Compat (Code (..), CodeQ, unsafeCodeCoerce, unTypeCode, joinCode, bindCode, bindCode_, liftCode, IsCode (..))

-- | Short alias for @'Code' 'Q'@
type C = CodeQ

#if MIN_VERSION_template_haskell(2,17,0)
#else
type Quote q = q ~ Q
#endif

liftTyped :: (TH.Lift a, Quote q) => a -> Code q a
#if MIN_VERSION_template_haskell(2,16,0)
liftTyped = Code . TH.liftTyped
#else
liftTyped = Code . unsafeTExpCoerce . TH.lift
#endif

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

instance (TH.Lift a, Quote q, Num a) => Num (Code q a) where
    fromInteger x = liftTyped (fromInteger x)

    x + y = toCode [|| $$(fromCode x) + $$(fromCode y) ||]
    x - y = toCode [|| $$(fromCode x) - $$(fromCode y) ||]
    x * y = toCode [|| $$(fromCode x) * $$(fromCode y) ||]

    abs    x = toCode [|| abs $$(fromCode x) ||]
    negate x = toCode [|| negate $$(fromCode x) ||]
    signum x = toCode [|| signum $$(fromCode x) ||]

instance (TH.Lift a, Quote q, IsString a) => IsString (Code q a) where
    fromString  = liftTyped . fromString

-------------------------------------------------------------------------------
-- GHC Splice
-------------------------------------------------------------------------------

-- | A type of GHC typed splices.
-- In some future GHC version it might be 'Code', but not yet.
#if MIN_VERSION_template_haskell(2,17,0)
type GHCCode a = Code Q a
#else
type GHCCode a = TExpQ a
#endif
