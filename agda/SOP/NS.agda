{-# OPTIONS --type-in-type #-}
module SOP.NS where

open import Data.List
open import Data.Sum

data NS {k : Set} (F : k → Set) : List k → Set where
  Z : ∀ {x xs} → F x     → NS F (x ∷ xs)
  S : ∀ {x xs} → NS F xs → NS F (x ∷ xs)

-- append for NS

injLeft : ∀ {k : Set} {f : k → Set} {xs ys : List k}
         → NS f xs → NS f (xs ++ ys)
injLeft (Z x)  = Z x
injLeft (S ns) = S (injLeft ns)

injRight : ∀ {k : Set} {f : k → Set} {xs ys : List k}
         → NS f ys → NS f (xs ++ ys)
injRight {xs = []}    ns = ns
injRight {xs = _ ∷ _} ns = S (injRight ns)

splitNS : ∀ {k : Set} {f : k → Set} {xs ys : List k}
        → NS f (xs ++ ys) → NS f xs ⊎ NS f ys
splitNS {xs = []}    ns     = inj₂ ns
splitNS {xs = _ ∷ _} (Z x)  = inj₁ (Z x)
splitNS {xs = _ ∷ _} (S ns) with splitNS ns
... | inj₁ l = inj₁ (S l)
... | inj₂ r = inj₂ r

mapNS2 : ∀ {k : Set} {f : k → Set} {xs : List k}
           {l : Set} {g : l → Set} {h : k → l}
       → (∀ {k} → f k → g (h k)) → NS f xs → NS g (Data.List.map h xs)
mapNS2 f (Z x)  = Z (f x)
mapNS2 f (S xs) = S (mapNS2 f xs)

mapNS3 : ∀ {k : Set} {f : k → Set} {xs : List k}
           {l : Set} {g : l → Set} {h : k → l}
       → (∀ {k} → g (h k) → f k) → NS g (Data.List.map h xs) → NS f xs
mapNS3 {xs = _ ∷ _} f (Z x)  = Z (f x)
mapNS3 {xs = _ ∷ _} f (S xs) = S (mapNS3 f xs)

mapNS : ∀ {k : Set} {f g : k → Set} {xs : List k}
      → (∀ {k} → f k → g k) → NS f xs → NS g xs
mapNS f (Z x)  = Z (f x)
mapNS f (S xs) = S (mapNS f xs)

-- flattening NS
module _ {k : Set} {f : k → Set} where
  flattenNS : {xss : List (List k)} → NS (NS f) xss → NS f (concat xss)
  flattenNS {[]} ()
  flattenNS {_ ∷ _} (Z ns) = injLeft ns
  flattenNS {_ ∷ _} (S ns) = injRight (flattenNS ns)

  unflattenNS : {xss : List (List k)} → NS f (concat xss) → NS (NS f) xss
  unflattenNS {[]} ()
  unflattenNS {_ ∷ _} ns with splitNS ns
  ... | inj₁ l = Z l
  ... | inj₂ r = S (unflattenNS r)
