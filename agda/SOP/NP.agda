{-# OPTIONS --type-in-type #-}
module SOP.NP where

open import Data.List

data NP {k : Set} (F : k → Set) : List k → Set where
  Nil  : NP F []
  _:*_ : ∀ {x xs} → F x → NP F xs → NP F (x ∷ xs)

infixr 5 _:*_

appendNP : ∀ {k : Set} {f : k → Set} {xs ys : List k}
          → NP f xs → NP f ys → NP f (xs ++ ys)
appendNP Nil       ys = ys
appendNP (x :* xs) ys = x :* appendNP xs ys

fstNP : ∀ {k : Set} {f : k → Set} {xs ys : List k}
      → NP f (xs ++ ys) → NP f xs
fstNP {xs = []}    _         = Nil
fstNP {xs = _ ∷ _} (x :* np) = x :* fstNP np

sndNP : ∀ {k : Set} {f : k → Set} {xs ys : List k}
      → NP f (xs ++ ys) → NP f ys
sndNP {xs = []}    np        = np
sndNP {xs = _ ∷ _} (_ :* np) = sndNP np

mapNP : ∀ {k : Set} {f g : k → Set} {xs : List k}
      → (∀ {k} → f k → g k) → NP f xs → NP g xs
mapNP f Nil       = Nil
mapNP f (x :* xs) = f x :* mapNP f xs

mapNP2 : ∀ {k : Set} {f : k → Set} {xs : List k}
           {l : Set} {g : l → Set} {h : k → l}
       → (∀ {k} → f k → g (h k)) → NP f xs → NP g (Data.List.map h xs)
mapNP2 f Nil       = Nil
mapNP2 f (x :* xs) = f x :* mapNP2 f xs

-- flattening NP
module _ {k : Set} {f : k → Set} where
  flattenNP : ∀ {xss} → NP (NP f) xss → NP f (concat xss)
  flattenNP Nil         = Nil
  flattenNP (xs :* xss) = appendNP xs (flattenNP xss)

  unflattenNP : ∀ {xss} → NP f (concat xss) → NP (NP f) xss
  unflattenNP {[]}    Nil = Nil
  unflattenNP {_ ∷ _} np  = fstNP np :* unflattenNP (sndNP np)
