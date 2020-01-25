{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
module Coutts where

import Staged.Stream.Step
data Coutts a = forall s. MkCoutts (s -> Step a s) s

cSum :: [Int] -> Int
cSum = foldl_s (+) 0 . stream
{-# INLINE cSum #-}

cReturn :: a -> [a]
cReturn = unstream . return_s
{-# INLINE cReturn #-}

cConcatMap :: (a -> [b]) -> [a] -> [b]
cConcatMap f = unstream . concatMap_s (stream . f) . stream
{-# INLINE cConcatMap #-}

cEnumFromTo :: Int -> Int -> [Int]
cEnumFromTo lo hi = unstream (enumFromTo_s lo hi)
{-# INLINE cEnumFromTo #-}

stream :: [a] -> Coutts a
stream xs0 = MkCoutts next xs0 where
    next []     = Stop
    next (x:xs) = Emit x xs
{-# INLINE [1] stream #-}

unstream :: Coutts a -> [a]
unstream (MkCoutts next xs0) = unfold xs0 where
    unfold s = case next s of
        Stop      ->     []
        Skip   s' ->     unfold s'
        Emit x s' -> x : unfold s'
{-# INLINE [1] unstream #-}

{-# RULES "stream/unstream" forall x. stream (unstream x) = x #-}

foldl_s :: (b -> a -> b) -> b -> Coutts a -> b
foldl_s f z0 (MkCoutts next s0) = go z0 s0 where
    go !z !s = case next s of
        Stop      -> z
        Skip   s' -> go z       s'
        Emit a s' -> go (f z a) s'
{-# INLINE foldl_s #-}

return_s :: a -> Coutts a
return_s x = MkCoutts next True where
    next True  = Emit x False
    next False = Stop
{-# INLINE return_s #-}

enumFromTo_s :: Int -> Int -> Coutts Int
enumFromTo_s lo hi = MkCoutts next lo where
    next n | n > hi    = Stop
           | otherwise = Emit n (1 + n)
{-# INLINE enumFromTo_s #-}

concatMap_s :: (a -> Coutts b) -> Coutts a -> Coutts b
concatMap_s f (MkCoutts nexta sa0) = MkCoutts next (sa0, Nothing) where
    next (sa, Nothing) = case nexta sa of
        Stop      -> Stop
        Skip   s' -> Skip (s', Nothing)
        Emit a s' -> Skip (s', Just (f a))
        
    next (sa, Just (MkCoutts nextb sb)) = case nextb sb of
        Stop      -> Skip   (sa, Nothing)
        Skip   s' -> Skip   (sa, Just (MkCoutts nextb s'))
        Emit b s' -> Emit b (sa, Just (MkCoutts nextb s'))
{-# INLINE concatMap_s #-}
