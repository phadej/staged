module Unicode.Types where

import Control.Monad  (ap, liftM)

-- | Strict identity
newtype I' a = I' { unI' :: a }

instance Functor I' where
    fmap = liftM

instance Applicative I' where
    pure = I'
    (<*>) = ap

instance Monad I' where
    I' x >>= f = x `seq` f x
