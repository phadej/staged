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
import Language.Haskell.TH        (Q, TExp)
import Language.Haskell.TH.Lib    (TExpQ)

#if MIN_VERSION_template_haskell(2,17,0)
import Language.Haskell.TH        (Code (..), CodeQ)
import Language.Haskell.TH.Syntax
       (bindCode, bindCode_, joinCode, liftCode, liftTyped, unTypeCode,
       unsafeCodeCoerce, Quote)
#else
import Language.Haskell.TH        (ExpQ)
import Language.Haskell.TH.Syntax (Exp, unTypeQ, unsafeTExpCoerce)
#endif

import qualified Control.Monad.Trans.Class  as Trans
import qualified Language.Haskell.TH.Syntax as TH

#if MIN_VERSION_template_haskell(2,17,0)
#else
-- | A 'Code' newtype as in https://github.com/ghc-proposals/ghc-proposals/pull/195
--
-- Yet I use constructor name 'C' for brevity.
newtype Code q a = Code { examineCode :: q (TExp a) }

type CodeQ = Code Q

-- |
type Quote q = q ~ Q
#endif


-- | Short alias for @'Code' 'Q'@
type C = CodeQ

-------------------------------------------------------------------------------
-- Conversions
-------------------------------------------------------------------------------

-- | Shorter alias for 'examineCode'.
unC :: Code Q a -> TExpQ a
unC = examineCode

#if __GLASGOW_HASKELL__ <811
liftCode :: q (TExp a) -> Code q a
liftCode = Code

liftTyped :: (TH.Lift a, Quote q) => a -> Code q a
#if MIN_VERSION_template_haskell(2,16,0)
liftTyped = Code . TH.liftTyped
#else
liftTyped = Code . unsafeTExpCoerce . TH.lift
#endif

unsafeCodeCoerce :: ExpQ -> Code Q a
unsafeCodeCoerce = liftCode . unsafeTExpCoerce

unTypeCode :: Code Q a -> Q Exp
unTypeCode (Code q) = unTypeQ q

joinCode :: Monad q => q (Code q a) -> Code q a
joinCode x = liftCode (x >>= examineCode)

bindCode :: Monad q => q a -> (a -> Code q b) -> Code q b
bindCode q k = liftCode (q >>= examineCode . k)

bindCode_ :: Monad q => q a -> Code q b -> Code q b
bindCode_ q c = liftCode (q >> examineCode c)
#endif

transCode :: (Trans.MonadTrans t, Monad q) => Code q a -> Code (t q) a
transCode (Code x) = Code (Trans.lift x)

-------------------------------------------------------------------------------
-- Questionable instances
-------------------------------------------------------------------------------

instance (TH.Lift a, Quote q, Num a) => Num (Code q a) where
    fromInteger x = liftTyped (fromInteger x)

#if __GLASGOW_HASKELL__ >=811
    x + y = [|| $$x + $$y ||]
    x - y = [|| $$x - $$y ||]
    x * y = [|| $$x * $$y ||]

    abs x = [|| abs $$x ||]
    negate x = [|| negate $$x ||]
    signum x = [|| signum $$x ||]
#else
    Code x + Code y = Code [|| $$x + $$y ||]
    Code x * Code y = Code [|| $$x * $$y ||]
    Code x - Code y = Code [|| $$x - $$y ||]

    abs    (Code x) = Code [|| abs $$x ||]
    negate (Code x) = Code [|| negate $$x ||]
    signum (Code x) = Code [|| signum $$x ||]
#endif

instance (TH.Lift a, Quote q, IsString a) => IsString (Code q a) where
    fromString  = liftTyped . fromString

-------------------------------------------------------------------------------
-- GHC Splice
-------------------------------------------------------------------------------

-- | A type of GHC typed splices.
-- In some future GHC version it might be 'Code', but not yet.
type GHCCode a = TExpQ a

class IsCode q a c | c -> a q where
    toCode   :: c -> Code q a
    fromCode :: Code q a -> c

instance Quote q => IsCode q a (Code q a) where
    toCode   = id
    fromCode = id

instance texp ~ TExp a => IsCode Q a (Q texp) where
    toCode   = liftCode
    fromCode = examineCode
