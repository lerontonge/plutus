---
title: (Declarative) Term Renaming and Substitution 
layout: page
---

```
module Declarative.RenamingSubstitution where
```

## Imports

```
open import Function using (_∘_)
open import Relation.Binary.PropositionalEquality using (refl;sym;trans)
open import Data.List using (map)

open import Utils using (Kind;*;K)
open import Type using (Ctx⋆;_⊢⋆_;Φ;Ψ;A;B)
open _⊢⋆_
import Type.RenamingSubstitution as ⋆
open import Type.Equality using (ren≡β;sub≡β;≡2β;_≡β_)
open _≡β_
open import Declarative using (Ctx;Γ;Δ;_∋_;conv∋;_⊢_;conv⊢;piBody;btype-ren;btype-sub;muPat;muArg;typeOf;typeOf∋)
open Ctx
open _∋_
open _⊢_
```


## Renaming

A term renaming maps every term variables in a context `Γ` to a
variable in context `Δ`. It is indexed by a type renaming which does
the same thing for type variables.

```
Ren : ∀ Γ Δ → ⋆.Ren Φ Ψ → Set
Ren Γ Δ ρ = ∀{A} → Γ ∋ A → Δ ∋ ⋆.ren ρ A
```

Extending a renaming to work on longer context extended by an
additional variable - used when going under a binder. It maps the new
variable `Z` to `Z` and shift everything else along.

```
ext : (ρ⋆ : ⋆.Ren Φ Ψ)
    → Ren Γ Δ ρ⋆
    → ∀{B}
      -------------------------------
    → Ren (Γ , B) (Δ , ⋆.ren ρ⋆ B) ρ⋆
ext _ ρ Z     = Z 
ext _ ρ (S x) = S (ρ x)
```

Extending a renaming to work on a context extended by one additional
type variable - used when going under a type binder. This doesn't
actually change any term variables, it is a case of managing the type
information.

```
ext⋆ : (ρ⋆ : ⋆.Ren Φ Ψ)
     → Ren Γ Δ ρ⋆
     → ∀ {K}
       --------------------------------
     → Ren (Γ ,⋆ K) (Δ ,⋆ K) (⋆.ext ρ⋆)
ext⋆ _ ρ (T x) = conv∋
  refl
  (trans (sym (⋆.ren-comp _)) (⋆.ren-comp _))
  (T (ρ x))
```

## Renaming for terms

```
ren : (ρ⋆ : ⋆.Ren Φ Ψ)
    → Ren Γ Δ ρ⋆
      -------------------------------
    → (∀{A} → Γ ⊢ A → Δ ⊢ ⋆.ren ρ⋆ A)
ren _ ρ (` x) = ` (ρ x)
ren _ ρ (ƛ L) = ƛ (ren _ (ext _ ρ) L)
ren _ ρ (L · M) = ren _ ρ L · ren _ ρ M 
ren _ ρ (Λ L) = Λ (ren _ (ext⋆ _ ρ) L)
ren ρ⋆ ρ (L ·⋆ A) =
  conv⊢ refl (⋆.ren-Π A (piBody L) ρ⋆) (ren _ ρ L ·⋆ ⋆.ren ρ⋆ A)
ren _ ρ (wrap A B L) = wrap _ _ (conv⊢ refl (⋆.ren-μ _ A B) (ren _ ρ L))
ren _ ρ (unwrap L) = conv⊢ refl (sym (⋆.ren-μ _ _ _)) (unwrap (ren _ ρ L))
ren _ ρ (conv p L) = conv (ren≡β _ p) (ren _ ρ L)
ren ρ⋆ ρ (con {A} cn p) = con {A = A} cn (trans≡β (ren≡β ρ⋆ p) (≡2β (sym (⋆.sub∅-ren A ρ⋆))))
ren ρ⋆ _ (builtin b) = conv⊢ refl (btype-ren b ρ⋆) (builtin b)
ren _ _ (error A) = error (⋆.ren _ A)
```

Weakening a term by an additional type variable

```
weaken : Γ ⊢ A
         ---------
       → Γ , B ⊢ A
weaken x = conv⊢
  refl
  (⋆.ren-id _)
  (ren _ (conv∋ refl (sym (⋆.ren-id _)) ∘ S) x)
```

```
weaken⋆ : Γ ⊢ A
          -------------------
        → Γ ,⋆ K ⊢ ⋆.weaken A
weaken⋆ x = ren _ T x
```

## Substitution

A substitution maps term variables to terms. It is indexed by a type
substitution.

```
Sub : ∀ Γ Δ → ⋆.Sub Φ Ψ → Set
Sub Γ Δ σ = ∀{A} → Γ ∋ A → Δ ⊢ ⋆.sub σ A
```

Extend a substitution by an additional term variable. Used for going
under binders.

```
exts : (σ⋆ : ⋆.Sub Φ Ψ)
     → Sub Γ Δ σ⋆
     → ∀ {B}
       -------------------------------
     → Sub (Γ , B) (Δ , ⋆.sub σ⋆ B) σ⋆
exts σ⋆ σ Z     = ` Z
exts σ⋆ σ (S x) = weaken (σ x)
```

Extend a substitution by an additional type variable. Used for going
under type binders.

```
exts⋆ : (σ⋆ : ⋆.Sub Φ Ψ)
      → Sub Γ Δ σ⋆
      → ∀{K}
        ---------------------------------
      → Sub (Γ ,⋆ K) (Δ ,⋆ K) (⋆.exts σ⋆) 
exts⋆ _ σ (T {A = A} x) = conv⊢
  refl
  (trans (sym (⋆.ren-sub A)) (⋆.sub-ren A))
  (weaken⋆ (σ x))
```

## Substitution for terms

```
sub : (σ⋆ : ⋆.Sub Φ Ψ)
    → Sub Γ Δ σ⋆
      -----------------------------
    → ∀{A} → Γ ⊢ A → Δ ⊢ ⋆.sub σ⋆ A
sub _  σ (` k)        = σ k
sub _  σ (ƛ L)        = ƛ (sub _ (exts _ σ) L)
sub _  σ (L · M)      = sub _ σ L · sub _ σ M
sub _  σ (Λ L)        = Λ (sub _ (exts⋆ _ σ) L)
sub σ⋆ σ (L ·⋆ A)     =
  conv⊢ refl (⋆.sub-Π A (piBody L) σ⋆) (sub σ⋆ σ L ·⋆ ⋆.sub σ⋆ A)
sub _  σ (wrap A B L) = wrap _ _ (conv⊢ refl (⋆.sub-μ _ A B) (sub _ σ L))
sub _  σ (unwrap L)   =
  conv⊢ refl (sym (⋆.sub-μ _ (muPat L) (muArg L))) (unwrap (sub _ σ L))
sub _  σ (conv p L)   = conv (sub≡β _ p) (sub _ σ L)
sub σ⋆ _ (con {A} cn p) = con {A = A} cn (trans≡β (sub≡β σ⋆ p) (≡2β (sym (⋆.sub∅-sub A σ⋆))))
sub _  _ (builtin b)  = conv⊢ refl (btype-sub b _) (builtin b)
sub _  _ (error A)    = error (⋆.sub _ A)
```

Extending a substitution by a term. Substitutions are implemented as
functions but they could also be implemented as lists. This operation
corresponds to cons (`_∷_`) for backwards lists.

```
subcons : (σ⋆ : ⋆.Sub Φ Ψ)
        → (Sub Γ Δ σ⋆)
        → ∀{A}
        → Δ ⊢ ⋆.sub σ⋆ A
          ----------------
        → Sub (Γ , A) Δ σ⋆
subcons _ σ L Z     = L
subcons _ σ L (S x) = σ x
```

Substitute a single variable in a term. Used to specify the beta-rule
for appliction in reduction.

```
_[_] : Γ , A ⊢ B
     → Γ ⊢ A
       ---------
     → Γ ⊢ B
L [ M ] = conv⊢
  refl
  (⋆.sub-id (typeOf L))
  (sub
    _
    (subcons ` (` ∘ conv∋ refl (sym (⋆.sub-id _)))
    (conv⊢ refl (sym (⋆.sub-id (typeOf M))) M)) L)
```

Substitute a single type variable in a term. Used to specify the
beta-rule for type instantiation in reduction.

```
_[_]⋆ : Γ ,⋆ K ⊢ B
      → (A : Φ ⊢⋆ K)
        -------------
      → Γ ⊢ B ⋆.[ A ]
L [ A ]⋆ = sub
  _
  (λ {(T x) →
        conv⊢ refl (trans (sym (⋆.sub-id _)) (⋆.sub-ren (typeOf∋ x))) (` x)})
  L
```
