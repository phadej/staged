{-# LANGUAGE FlexibleContexts #-}
module Examples where

import Staged.Commons

ex01 :: IsCode Bool cond => cond -> GHCCode Int
ex01 b = fromCode $ sIfThenElse (toCode b) (sint 1) (sint 2)
