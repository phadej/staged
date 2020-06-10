{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -dsuppress-all #-}
-- {-# OPTIONS -ddump-splices #-}
module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Property, (===), Fun, applyFun)
import Test.QuickCheck.Poly (A, B)
import Control.Monad ((>=>))
import Control.Category ((>>>))

import qualified Test.QuickCheck.Poly

import Staged.Commons
import qualified Staged.Stream.Pure as S
import qualified Staged.Stream.Pure.Examples as S

type C' = Test.QuickCheck.Poly.C

main :: IO ()
main = defaultMain $ testGroup "QuickCheck"
    -- Note: this strictifies the spine
    [ testProperty "toList . fromList = spineStrict" $
        let prop :: [A] -> Property
            prop xs = xs ===
                $$(S.toList sunit $ S.fromList $ const $ toCode [|| xs ||])
        in prop

    , testProperty "toList . fromList" $
        let prop :: Fun A [B] -> A -> Property
            prop f x = applyFun f x ===
                $$(S.toList [|| x ||] $ S.fromList [|| applyFun f ||])
        in prop

    , testProperty ">>>" $
        let prop :: Fun A [B] -> Fun B [C'] -> A -> Property
            prop f g x = (applyFun f >=> applyFun g) x ===
                $$(S.toList [|| x ||] $ S.fromList [|| applyFun f ||] >>> S.fromList [|| applyFun g ||])
        in prop

    , testProperty "map" $
        let prop :: Fun A B -> [A] -> Property
            prop f xs = map (applyFun f) xs ===
                $$(S.toList sunit $ S.map [|| applyFun f ||]
                          $ S.fromList $ sconst [|| xs ||])
        in prop

    , testProperty "filter" $
        let prop :: Fun A Bool -> [A] -> Property
            prop f xs = filter (applyFun f) xs ===
                $$(S.toList sunit $ S.filter [|| applyFun f ||]
                                  $ S.fromList $ sconst [|| xs ||])
        in prop

    , testProperty "take" $
        let prop :: Int -> [A] -> Property
            prop n xs = take n xs ===
                $$(S.toList sunit $ S.take [|| n ||] $ S.fromList $ sconst [|| xs ||])
        in prop

    , testProperty "drop" $
        let prop :: Int -> [A] -> Property
            prop n xs = drop n xs ===
                $$(S.toList sunit $ S.drop [|| n ||] $ S.fromList $ sconst [|| xs ||])
        in prop

    , testProperty "bfsTree" $
        let prop :: Property
            prop = xs === [1,2,3,4,4,3,4,4,2,3,4,4,3,4,4]

            xs = $$(S.toList (sint 0) $ S.bfsTree
                (S.drop (sint 1) $ S.enumFromTo'' (sint 4))
                (\_ -> strue))
        in prop

    , testProperty "zipWith" $
        let prop :: [Int] -> [Int] -> Property
            prop xs ys = zipWith (+) xs ys ===
                $$(S.toList sunit $ S.zipWith [|| (+) ||]
                    (S.fromList $ sconst [|| xs ||])
                    (S.fromList $ sconst [|| ys ||]))
        in prop

    , testProperty "zipWith" $
        let prop :: [Int] -> [Int] -> Property
            prop xs ys = alignWith id id (+) xs ys ===
                $$(S.toList sunit $ S.alignWith sid sid [|| (+) ||]
                    (S.fromList $ sconst [|| xs ||])
                    (S.fromList $ sconst [|| ys ||]))
        in prop
    ]

alignWith :: (a -> c) -> (b -> c) -> (a -> b -> c) -> [a] -> [b] -> [c]
alignWith f g h = aux where
    aux []     []     = []
    aux []     ys     = map g ys
    aux xs     []     = map f xs
    aux (x:xs) (y:ys) = h x y : aux xs ys
