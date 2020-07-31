{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Staged.Kleene (
    compileRE,
) where

import Kleene.RE
import Kleene.Internal.Partition
import Data.Traversable (for)
import Staged.Commons

import qualified Data.Set as Set
import qualified Data.Map as Map

compileRE :: RE Char -> Code Q ([Char] -> Bool)
compileRE re = sletrec aux re

aux :: forall m. Monad m
    => (RE Char -> m (Code Q ([Char] -> Bool)))
    -> (RE Char -> m (Code Q ([Char] -> Bool)))
aux _   re
    | isEmpty re
    = return $ fromFn $ \_ -> sfalse
aux rec re = do
    next' <- fmap Map.fromList $ for (Set.toList $ examples lc) $ \c -> do
        re' <- rec (derivate c re)
        return (c, re')

    let next :: Char -> Code Q [Char] -> Code Q Bool
        next c xs = case Map.lookup c next' of
            Nothing -> sfalse
            Just k  -> k @@ xs

    return $ fromFn $ \input -> scaseList input
        (if nullable re then strue else sfalse)
        $ \x xs -> partitionCase lc (\c -> next c xs) x
  where
    lc = leadingChars re

partitionCase
    :: forall r.
       Partition Char
    -> (Char -> Code Q r)
    -> (Code Q Char -> Code Q r)
partitionCase p k c = go (Set.toList $ examples p) where
    go :: [Char] -> Code Q r
    go []  = error "empty examples"
    go [x] = k x
    go (x:xs) = sIfThenElse (toCode [|| $$(fromCode c) <= x ||])
        (k x)
        (go xs)
