---
title: Erasure of (Declarative) terms
layout: page
---

```
module Declarative.Erasure where
```

## Imports

```
open import Data.Nat using (ℕ)
open import Data.Empty using (⊥)
open import Data.List using (map)
open import Data.Unit using (tt)
open import Relation.Binary.PropositionalEquality using (refl;subst)

open import Declarative using (Ctx;_∋_;_⊢_;ty2TyTag;⟦_⟧d)
open Ctx
open _∋_
open _⊢_
import Declarative.RenamingSubstitution as D
open import Type using (Ctx⋆;∅;_,⋆_;_⊢⋆_)
open _⊢⋆_
import Type.RenamingSubstitution as T
open import Untyped using (_⊢)
open _⊢
import Untyped.RenamingSubstitution as U
open import Utils using (Kind;♯;*;Maybe;nothing;just;fromList)
open import RawU using (TmCon;tmCon;TyTag)
open import Builtin.Signature using (_⊢♯) 
open import Builtin.Constant.Type

open import Type.BetaNBE using (nf)
open import Algorithmic using (ty≅sty₁)
```

```
len : ∀{Φ} → Ctx Φ → Set
len ∅        = ⊥
len (Γ ,⋆ K) = len Γ
len (Γ , A)  = Maybe (len Γ)

lenI : ∀{Φ} → Ctx Φ → Set
lenI ∅        = ⊥
lenI (Γ ,⋆ K) = Maybe (lenI Γ)
lenI (Γ , A)  = Maybe (lenI Γ)

len⋆ : Ctx⋆ → Set
len⋆ ∅        = ⊥
len⋆ (Γ ,⋆ K) = Maybe (len⋆ Γ)

eraseVar : ∀{Φ Γ}{A : Φ ⊢⋆ *} → Γ ∋ A → len Γ
eraseVar Z     = nothing
eraseVar (S α) = just (eraseVar α)
eraseVar (T α) = eraseVar α

eraseTC : (A : ∅ ⊢⋆ ♯) → ⟦ A ⟧d → TmCon
eraseTC A t = tmCon (ty2TyTag A) (subst Algorithmic.⟦_⟧ (ty≅sty₁ (nf A)) t) 

erase : ∀{Φ Γ}{A : Φ ⊢⋆ *} → Γ ⊢ A → len Γ ⊢

erase-Sub : ∀{Φ Ψ}{Γ : Ctx Φ}{Δ : Ctx Ψ}(σ⋆ : T.Sub Φ Ψ)
  → D.Sub Γ Δ σ⋆ → U.Sub (len Γ) (len Δ) 

erase (` α)           = ` (eraseVar α)
erase (ƛ t)           = ƛ (erase t) 
erase (t · u)         = erase t · erase u
erase (Λ t)           = delay (erase t)
erase (t ·⋆ A)        = force (erase t)
erase (wrap A B t)    = erase t
erase (unwrap t)      = erase t
erase (conv p t)      = erase t
erase (con {A = A} t _) = con (eraseTC A t)
erase (builtin b)     = builtin b
erase (error A)       = error

backVar⋆ : ∀{Φ}(Γ : Ctx Φ) → len Γ → Φ ⊢⋆ *
backVar⋆ (Γ ,⋆ J) x       = T.weaken (backVar⋆ Γ x)
backVar⋆ (Γ , A) nothing  = A
backVar⋆ (Γ , A) (just x) = backVar⋆ Γ x

backVar : ∀{Φ}(Γ : Ctx Φ)(x : len Γ) → Γ ∋ (backVar⋆ Γ x)
backVar (Γ ,⋆ J) x        = T (backVar Γ x)
backVar (Γ , A)  nothing  = Z
backVar (Γ , A)  (just x) = S (backVar Γ x)

erase-Sub σ⋆ σ i = erase (σ (backVar _ i))
```
