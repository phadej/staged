-- | Pure staged streams.
module Staged.Stream.Pure (
    -- * Stream
    Stream (..),
    Step (..),
    mkStream,
    -- * Combinators
    --
    -- | Recall that @'Stream' a b@ is representing some @a -> [b]@ function.
    --
    -- Then we can compare 'Prelude' and @staged-streams@ functions.
    -- Take for example 'Prelude.iterate'
    --
    -- @
    -- 'Prelude.iterate' :: (a -> a) -> a -> [a]
    -- @
    --
    -- Now, if we stage the argument function, and treat the trailing @a -> [a]@
    -- as @'Stream' a a@ we get a combinator of the same name in @staged-streams@:
    --
    -- @
    -- 'iterate' :: ('C' a -> 'C' a) -> 'Stream' a a
    -- @
    --
    -- In practice the real type of 'iterate'
    -- is a bit more complicated, but the above is one instantiation
    -- of more polymorphic real type. That allows to use spliced functions directly.
    --
    -- >>> $$(toList (sint 5) $ take (sint 10) $ iterate [|| succ ||])
    -- [5,6,7,8,9,10,11,12,13,14]
    --
    -- You may wonder why the 'toList' combinator takes an additional argument:
    --
    -- @
    -- 'toList' :: 'C' a -> 'Stream' a b -> 'GHCCode' [b]
    -- @
    --
    -- Remember still that @'Stream' a b@ is like @a -> [b]@ function.
    -- Therefore we need a value to get the resulting list.
    -- So 'toList' is more akin to reverse function application.
    --
    -- @
    -- ('Data.Function.&') :: a -> (a -> [b]) -> [b]
    -- @
    --
    -- Yet, we think 'toList' is better name.
    -- After all, 'toList' produces a list.
    --
    -- Some combinators, like 'replicate' should have variants
    -- where either of arguments comes as an input.
    -- We don't know how to name these additional combinators:
    --
    -- @
    -- 'replicate' :: 'C' Int -> 'C' b -> 'Stream' a   b
    -- ???       :: 'C' Int ->        'Stream' b   b
    -- ???       ::          'C' b -> 'Stream' Int b
    -- @
    --
    -- Therefore we leave them out.
    --
    -- Ultimately we'll need N-ary input/output streams, so we could have
    -- fourth variant, and better type for 'replicate':
    --
    -- @
    -- ???       ::                 StreamN [Int, b] [b]
    -- replicate :: 'C' Int -> 'C' b -> StreamN []       [b]
    -- @
    --
    -- but we left that stream variant out from the initial release of @staged-streams@.
    --
    -- === API design guidelines
    --
    -- * Function arguments are overloaded using classes
    --   'Staged.Compat.IsCode', 'Staged.Commons.ToCodeFn' and 'Staged.Commons.ToCodeFn2'.
    --   This allows using 'Staged.Compat.Code', as well
    --   current GHC's type of quotes expressions @TExpQ@ as argument to combinators.
    -- * Stream eliminators return 'Staged.Compat.GHCCode',
    --   so they can be spliced directly.
    -- * It's possible to compose 'Stream's using 'Control.Category.Category'
    --   instance. However we recommend to use stream transformers when possible
    --   (e.g. 'map' over 'mapPipe') as stream transformers generate
    --   smaller code.
    --
    -- In summary: two first principles mean that combinators have concrete output types,
    -- and polymorphic inputs types.
    -- Being consistent in this way means that we don't get ambiguous types errors.
    --
    module Staged.Stream.Pure.Combinators,
) where

import Prelude ()

import Staged.Stream.Pure.Type
import Staged.Stream.Pure.Convenience
import Staged.Stream.Pure.Combinators

-- $setup
-- >>> import Staged.Commons
-- >>> import Prelude (($), succ)
-- >>> :set -XTemplateHaskell
