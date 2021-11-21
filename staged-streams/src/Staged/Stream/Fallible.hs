-- | Monadic staged streams.
module Staged.Stream.Fallible (
    -- * Stream
    StreamFM (..),
    FallibleStep (..),
    mkStreamFM,
    -- * Combinators
    --
    -- | See "Staged.Stream.Pure" for introduction to combinators.
    --   Remember that monadic 'StreamM' is like @a -> ListT m b@ function,
    --   and not for @a -> m [b]@ or @a -> [m b]@.
    module Staged.Stream.Fallible.Combinators,
    -- * Utilities
    sletrec_SOP,
    sletrec1_SOP,
    ) where

import Staged.Stream.Fallible.Type
import Staged.Stream.Fallible.Convenience
import Staged.Stream.Fallible.Combinators
import Staged.Stream.Internal
