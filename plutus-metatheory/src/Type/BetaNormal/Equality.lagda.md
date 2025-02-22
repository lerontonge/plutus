```
module Type.BetaNormal.Equality where
```

## Imports

```
open import Function using (id;_∘_)
open import Relation.Binary.PropositionalEquality using (_≡_;refl;trans;cong;cong₂)

open import Utils using (*;J)
open import Type using (Ctx⋆;Θ;Φ;Ψ;_∋⋆_)
open import Type.BetaNormal using (_⊢Nf⋆_;_⊢Ne⋆_;renNf;renNe)
open _⊢Nf⋆_
open _⊢Ne⋆_
open import Type.RenamingSubstitution using (Ren;ext-cong;ext-id;ext-comp)
```

```
renNf-cong : {f g : Ren Φ Ψ}
           → (∀ {J}(α : Φ ∋⋆ J) → f α ≡ g α)
           → ∀{K}(A : Φ ⊢Nf⋆ K)
             -------------------------------
           → renNf f A ≡ renNf g A
renNe-cong : {f g : Ren Φ Ψ}
           → (∀ {J}(α : Φ ∋⋆ J) → f α ≡ g α)
           → ∀{K}(A : Φ ⊢Ne⋆ K)
             -------------------------------
           → renNe f A ≡ renNe g A

renNf-cong p (Π A)   = cong Π (renNf-cong (ext-cong p) A)
renNf-cong p (A ⇒ B) = cong₂ _⇒_ (renNf-cong p A) (renNf-cong p B)
renNf-cong p (ƛ A)   = cong ƛ (renNf-cong (ext-cong p) A)
renNf-cong p (ne A)  = cong ne (renNe-cong p A)
renNf-cong p (con c) = cong con (renNf-cong p c)
renNf-cong p (μ A B) = cong₂ μ (renNf-cong p A) (renNf-cong p B)

renNe-cong p (` α)   = cong ` (p α)
renNe-cong p (A · B) = cong₂ _·_ (renNe-cong p A) (renNf-cong p B)
renNe-cong p (^ x)   = refl
```

```
renNf-id : (n : Φ ⊢Nf⋆ J)
           --------------
         → renNf id n ≡ n
renNe-id : (n : Φ ⊢Ne⋆ J)
           --------------
         → renNe id n ≡ n

renNf-id (Π A)   = cong Π (trans (renNf-cong ext-id A) (renNf-id A))
renNf-id (A ⇒ B) = cong₂ _⇒_ (renNf-id A) (renNf-id B)
renNf-id (ƛ A)   = cong ƛ (trans (renNf-cong ext-id A) (renNf-id A))
renNf-id (ne A)  = cong ne (renNe-id A)
renNf-id (con c) = cong con (renNf-id c)
renNf-id (μ A B) = cong₂ μ (renNf-id A) (renNf-id B)

renNe-id (` α)   = refl
renNe-id (A · B) = cong₂ _·_ (renNe-id A) (renNf-id B)
renNe-id (^ x)   = refl
```

```
renNf-comp : {g : Ren Φ Ψ}
           → {f : Ren Ψ Θ}
           → ∀{J}(A : Φ ⊢Nf⋆ J)
             -------------------------------------
           → renNf (f ∘ g) A ≡ renNf f (renNf g A)
renNe-comp : {g : Ren Φ Ψ}
           → {f : Ren Ψ Θ}
           → ∀{J}(A : Φ ⊢Ne⋆ J)
             -------------------------------------
           → renNe (f ∘ g) A ≡ renNe f (renNe g A)

renNf-comp (Π B)   = cong Π (trans (renNf-cong ext-comp B) (renNf-comp B))
renNf-comp (A ⇒ B) = cong₂ _⇒_ (renNf-comp A) (renNf-comp B)
renNf-comp (ƛ A)   = cong ƛ (trans (renNf-cong ext-comp A) (renNf-comp A))
renNf-comp (ne A)  = cong ne (renNe-comp A)
renNf-comp (con c) = cong con (renNf-comp c)
renNf-comp (μ A B) = cong₂ μ (renNf-comp A) (renNf-comp B)

renNe-comp (` x)   = refl
renNe-comp (A · B) = cong₂ _·_ (renNe-comp A) (renNf-comp B)
renNe-comp (^ x)   = refl
```
 