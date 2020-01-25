{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}
module Staged.Compat (
    -- * Code
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
    -- * GHC Splice
    GHCCode,
    IsCode (..),
) where

import Data.String                (IsString (..))
import Language.Haskell.TH        (Exp, Q, TExp)
import Language.Haskell.TH.Lib    (ExpQ, TExpQ)
import Language.Haskell.TH.Syntax (unTypeQ, unsafeTExpCoerce)

import qualified Language.Haskell.TH.Syntax as TH
import qualified Generics.SOP                as SOP
import qualified GHC.Generics                as GHC

-- | A 'Code' newtype as in https://github.com/ghc-proposals/ghc-proposals/pull/195
--
-- Yet I use constructor name 'C' for brevity.
newtype Code m a = C { examineCode :: m (TExp a) }
  deriving (GHC.Generic)

instance SOP.Generic (Code m a)

-- | Short alias for @'Code' 'Q'@
type C = Code Q

-------------------------------------------------------------------------------
-- Conversions
-------------------------------------------------------------------------------

-- | Shorter alias for 'examineCode'.
unC :: Code Q a -> TExpQ a
unC = examineCode

liftCode :: m (TExp a) -> Code m a
liftCode = C

liftTyped :: TH.Lift a => a -> C a
#if MIN_VERSION_template_haskell(2,16,0)
liftTyped = C . TH.liftTyped
#else
liftTyped = C . unsafeTExpCoerce . TH.lift
#endif

unsafeCodeCoerce :: ExpQ -> Code Q a
unsafeCodeCoerce = liftCode . unsafeTExpCoerce

unTypeCode :: Code Q a -> Q Exp
unTypeCode (C m) = unTypeQ m

joinCode :: Monad m => m (Code m a) -> Code m a
joinCode x = liftCode (x >>= examineCode)

bindCode :: Monad m => m a -> (a -> Code m b) -> Code m b
bindCode q k = liftCode (q >>= examineCode . k)

bindCode_ :: Monad m => m a -> Code m b -> Code m b
bindCode_ q c = liftCode (q >> examineCode c)

-------------------------------------------------------------------------------
-- Questionable instances
-------------------------------------------------------------------------------

instance (TH.Lift a, m ~ Q, Num a) => Num (Code m a) where
    fromInteger x = liftTyped (fromInteger x)

    C x + C y = C [|| $$x + $$y ||]
    C x * C y = C [|| $$x * $$y ||]
    C x - C y = C [|| $$x - $$y ||]

    abs    (C x) = C [|| abs $$x ||]
    negate (C x) = C [|| negate $$x ||]
    signum (C x) = C [|| signum $$x ||]

instance (TH.Lift a, m ~ Q, IsString a) => IsString (Code m a) where
    fromString  = liftTyped . fromString

-------------------------------------------------------------------------------
-- GHC Splice
-------------------------------------------------------------------------------

-- | A type of GHC typed splices.
-- In some future GHC version it might be 'Code', but not yet.
type GHCCode a = TExpQ a

class IsCode a c | c -> a where
    toCode   :: c -> Code Q a
    fromCode :: Code Q a -> c

instance m ~ Q => IsCode a (Code m a) where
    toCode   = id
    fromCode = id

instance texp ~ TExp a => IsCode a (Q texp) where
    toCode   = liftCode
    fromCode = unC
