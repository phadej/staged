{-# OPTIONS --type-in-type #-}
module Stream where

open import Data.Unit
open import Data.List
open import Data.Sum
open import Data.Maybe
open import Data.Nat
open import Function using (_∘_)

open import SOP.NS
open import SOP.NP

-- injections

Injection : ∀ {k : Set} → (k → Set) → List k → k → Set
Injection f xs x = f x → NS f xs

injections : ∀ {k : Set} {f : k → Set} {xs}
           → NP (Injection f xs) xs
injections {xs = []}     = Nil
injections {xs = x ∷ xs} = Z :* (mapNP (λ f ns → S (f ns)) injections)

-- function exists to forall

module _ {k : Set} {f : k → Set}  {y : Set} where
  unfanIn : {xs : List k} → (NS f xs → y) → NP (λ x → f x → y) xs
  unfanIn {[]}    g = Nil
  unfanIn {_ ∷ _} g = (λ z → g (Z z)) :* (unfanIn (λ z → g (S z)))

  fanIn : {xs : List k} → NP (λ x → f x → y) xs → NS f xs → y
  fanIn Nil       ()
  fanIn (g :* gs) (Z x)  = g x
  fanIn (g :* gs) (S ns) = fanIn gs ns

module Mult {k : Set} {f : k → Set} where
  private
    lemma : ∀ {x ys} → f x → NS (NP f) ys → NS (NP f) (Data.List.map (x ∷_) ys)
    lemma x (Z y)  = Z (x :* y)
    lemma x (S ys) = S (lemma x ys)

    unlemma1 : ∀ {x ys} → NS (NP f) (Data.List.map (x ∷_) ys) → f x
    unlemma1 {ys = _ ∷ _} (Z (x :* _)) = x
    unlemma1 {ys = _ ∷ _} (S ns)       = unlemma1 ns

    unlemma2 : ∀ {x ys} → NS (NP f) (Data.List.map (x ∷_) ys) → NS (NP f) ys
    unlemma2 {ys = _ ∷ _} (Z (_ :* xs)) = Z xs
    unlemma2 {ys = _ ∷ _} (S xs)        = S (unlemma2 xs)

  fn : ∀ {xs ys} → NS f xs → NS (NP f) ys → NS (NP f) (concatMap (λ x → Data.List.map (x ∷_) ys) xs)
  fn (Z x)  ys = injLeft (lemma x ys)
  fn (S xs) ys = injRight (fn xs ys)

  unfn1 : ∀ {xs ys} → NS (NP f) (concatMap (λ x → Data.List.map (x ∷_) ys) xs) → NS f xs
  unfn1 {[]} ()
  unfn1 {x ∷ xs} {ys} ns with splitNS {xs = Data.List.map (_∷_ x) ys} ns
  ... | inj₁ l = Z (unlemma1 l)
  ... | inj₂ r = S (unfn1 r)

  unfn2 : ∀ {xs ys} → NS (NP f) (concatMap (λ x → Data.List.map (x ∷_) ys) xs) → NS (NP f) ys
  unfn2 {[]} ()
  unfn2 {x ∷ xs} {ys} ns with splitNS {xs = Data.List.map (_∷_ x) ys} ns
  ... | inj₁ l = unlemma2 l
  ... | inj₂ r = unfn2 {xs} r

sequence : ∀ {X} → List (List X) → List (List X)
sequence []         = [] ∷ []
sequence (xs ∷ xss) = concatMap (λ x → Data.List.map (x ∷_) (sequence xss)) xs

module _ {k : Set} {f : k → Set} where
  distrNPNS : ∀ {xss} → NP (NS f) xss → NS (NP f) (sequence xss)
  distrNPNS Nil         = Z Nil
  distrNPNS (xs :* xss) = Mult.fn xs (distrNPNS xss)

  undistrNPNS : ∀ {xss} → NS (NP f) (sequence xss) → NP (NS f) xss
  undistrNPNS {[]}       _  = Nil
  undistrNPNS {xs ∷ xss} ns =
    Mult.unfn1 ns :*
    undistrNPNS (Mult.unfn2 {xs = xs} ns)

FLATTEN : ∀ {X : Set} → List (List (List (List X))) → List (List X)
FLATTEN = concatMap (Data.List.map concat ∘ sequence)

module _ {k : Set} {f : k → Set} where
  flattenSOP : ∀ {xssss} → NS (NP (NS (NP f))) xssss → NS (NP f) (FLATTEN xssss)
  flattenSOP = flattenNS ∘ mapNS2 (mapNS2 flattenNP ∘ distrNPNS)

  unflattenSOP : ∀ {xssss} → NS (NP f) (FLATTEN xssss) → NS (NP (NS (NP f))) xssss
  unflattenSOP = mapNS3 (undistrNPNS ∘ mapNS3 unflattenNP) ∘ unflattenNS

-- ConcatMap1

mapCons : ∀ {X} → List X → List (List X) → List (List X)
mapCons xs []         = []
mapCons xs (ys ∷ yss) = (xs ++ ys) ∷ mapCons xs yss

concatMap1 : ∀ {X} → List (List X) → List (List X) → List (List X)
concatMap1 []         yss = []
concatMap1 (xs ∷ xss) yss = xs ∷ mapCons xs yss ++ concatMap1 xss yss

injectLeft : ∀ {k : Set} {f : k → Set} {xss yss}
           → NS (NP f) xss → NS (NP f) (concatMap1 xss yss)
injectLeft (Z x)  = Z x
injectLeft (S ns) = S (injRight (injectLeft ns))

weaken : ∀ {k : Set} {f : k → Set} {ys yss xss}
         → NS (NP f) (concatMap1 xss yss)
         → NS (NP f) (concatMap1 xss (ys ∷ yss))
weaken {xss = _ ∷ _} (Z x)  = Z x
weaken {yss = yss} {xss = xs ∷ xss} (S ns) with splitNS {xs = mapCons xs yss} ns
... | inj₁ l = S (S (injLeft l))
... | inj₂ r = S (S (injRight (weaken r)))

lemma1 : ∀ {k : Set} {f : k → Set} {xs yss}
       → NP f xs → NS (NP f) yss
       → NS (NP f) (mapCons xs yss)
lemma1 xs (Z ys)  = Z (appendNP xs ys)
lemma1 xs (S yss) = S (lemma1 xs yss)

lemma2 : ∀ {k : Set} {f : k → Set} {ys yss xss}
        → NS (NP f) xss → NP f ys
        → NS (NP f) (concatMap1 xss (ys ∷ yss))
lemma2 (Z xs)  ys = S (Z (appendNP xs ys))
lemma2 (S xss) ys = S (S (injRight (lemma2 xss ys)))

injectRight
  : ∀ {k : Set} {f : k → Set} {xss yss}
  → NS (NP f) yss
  → NS (NP f) xss
  → NS (NP f) (concatMap1 xss yss)
injectRight (Z ys) (Z xs) = S (Z (appendNP xs ys))
injectRight (Z ys) (S xs) = S (S (injRight (lemma2 xs ys)))
injectRight (S ys) (Z xs) = S (S (injLeft (lemma1 xs ys)))
injectRight (S ys) (S xs) = S (S (injRight (weaken (injectRight ys xs))))

-- Step

data Step (A : Set) (S : Set) : Set where
  stop  : Step A S
  skip  : S → Step A S
  yield : A → S → Step A S

mapStep₂ : ∀ {A S₁ S₂} → (S₁ → S₂) → Step A S₁ → Step A S₂
mapStep₂ f stop        = stop
mapStep₂ f (skip x)    = skip (f x)
mapStep₂ f (yield x s) = yield x (f s)

-- Stream

module Stream
  (Code : Set → Set)
  (caseCodeList : ∀ {A R} → Code (List A) → Code R → (Code A → Code (List A) → Code R) → Code R)
  where

  Loop : Set → List (List Set) → List Set → Set
  Loop B conf xs = (R : Set) → NP Code xs → (Step (Code B) (NS (NP Code) conf) → Code R) → Code R

  mapLoop₂ : ∀ {C} {xss₁ xss₂ ys}
           → (NS (NP Code) xss₁ → NS (NP Code) xss₂)
           → Loop C xss₁ ys → Loop C xss₂ ys
  mapLoop₂ f loop R ys k = loop R ys λ step → k (mapStep₂ f step)

  record Stream (A : Set) (B : Set) : Set where
    constructor stream
    field
      conf : List (List Set) -- configuration
      init : Code A → NS (NP Code) conf
      step : NP (Loop B conf) conf

  fromList : ∀ {A B} → (Code A → Code (List B)) → Stream A B
  fromList {A = A} {B = B} f = stream
    ((List B ∷ []) ∷ [])
    (λ a → Z (f a :* Nil))
    (step :* Nil)
    where
      step : ∀ R
           → NP Code (List B ∷ [])
           → (Step (Code B) (NS (NP Code) ((List B ∷ []) ∷ [])) → Code R)
           → Code R
      step _ (bs :* Nil) k = caseCodeList bs
        (k stop)
        λ x xs → k (yield x (Z (xs :* Nil)))

  compose : ∀ A B C → Stream A B → Stream B C → Stream A C
  compose A B C (stream xss x0 x-steps) (stream yss y0 y-steps) = stream
    (concatMap1 xss yss)
    z0
    (step xss x-steps injections)
    where
    z0 : Code A → NS (NP Code) (concatMap1 xss yss)
    z0 a = injectLeft (x0 a)

    step : ∀ xss'
         → NP (Loop B xss) xss'
         → NP (Injection (NP Code) xss) xss'
         → NP (Loop C (concatMap1 xss yss)) (concatMap1 xss' yss)
    step []          Nil               Nil           = Nil
    step (xs ∷ xss') (x-step :* steps) (inj :* injs) =
      corner :*
      appendNP
      (line y-steps injections)
      (step xss' steps injs)
      where
      corner : Loop C (concatMap1 xss yss) xs
      corner R codes k = x-step R codes λ
        { stop              → k stop
        ; (skip code)       → k (skip (injectLeft code))
        ; (yield b codes')  → k (skip (injectRight (y0 b) codes'))
        }

      line : ∀ {yss'}
           → NP (Loop C yss) yss'
           → NP (Injection (NP Code) yss) yss'
           → NP (Loop C (concatMap1 xss yss)) (mapCons xs yss')
      line {[]}        Nil Nil  = Nil
      line {ys ∷ yss'} (y-step :* steps) (injY :* injYs) = appendNP
        (border :* Nil)
        (line {yss' = yss'} steps injYs)
        where
        border : Loop C (concatMap1 xss yss) (xs ++ ys)
        border R xs++ys k = y-step R (sndNP xs++ys) λ
          { stop             → k (skip (injectLeft xs'))
          ; (skip codes')    → k (skip (injectRight codes' xs'))
          ; (yield c codes') → k (yield c (injectRight codes' xs'))
          }
          where
            xs' : NS (NP Code) xss
            xs' = inj (fstNP xs++ys)

module Example where
  caseList : {A R : Set} → List A → R → (A → List A → R) → R
  caseList []      nil _    = nil
  caseList (x ∷ xs) _  cons = cons x xs

  open Stream (λ X → X) caseList

  apply : ∀ {k} {f g : k → Set} {xs} {Y : Set}
        → (∀ {x} → f x → g x → Y) → NS f xs → NP g xs → Y
  apply nt (Z x)  (y :* _)  = nt x y
  apply nt (S ns) (_ :* np) = apply nt ns np

  toList : ∀ {A B} → ℕ → A → Stream A B → Maybe (List B)
  toList {B = B} gas0 a (stream conf init step) = go gas0 (init a)
    where
    go : ℕ → NS (NP (λ X → X)) conf → Maybe (List B)
    go zero st      = nothing
    go (suc gas) st with apply (λ np loop → loop (Maybe (List B)) np) st step
    ... | p = p λ
      { stop          → just []
      ; (skip st')    → go gas st'
      ; (yield b st') → Data.Maybe.map (b ∷_) (go gas st')
      }

  open import Relation.Binary.PropositionalEquality using (_≡_; refl)

  toZero : ℕ → List ℕ
  toZero zero    = zero ∷ []
  toZero (suc n) = suc n ∷ toZero n

  streamA : Stream ℕ ℕ
  streamA = fromList toZero

  streamAA = compose _ _ _ streamA streamA

  example₁ : toList 30 3 streamAA ≡ just (3 ∷ 2 ∷ 1 ∷ 0 ∷ 2 ∷ 1 ∷ 0 ∷ 1 ∷ 0 ∷ 0 ∷ [])
  example₁ = refl
