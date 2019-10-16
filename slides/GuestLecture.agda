module GuestLecture where

data Either (A : Set) (B : Set) : Set where
  Left  : A → Either A B
  Right : B → Either A B

data ⊥ : Set where

data ℕ : Set where
  Z : ℕ
  S : ℕ → ℕ

_+_ : ℕ → ℕ → ℕ
Z + n = n
(S m) + n = S (m + n)

{-# BUILTIN NATURAL ℕ #-}

data _≤_ : ℕ → ℕ → Set where
  z≤n : ∀{n}   → Z ≤ n
  s≤s : ∀{n m} → n ≤ m → S n ≤ S m

compare : (n m : ℕ) → Either (m ≤ n) (n ≤ m)
compare Z m = Right z≤n
compare (S n) Z = Left z≤n
compare (S n) (S m) with compare n m
...| Left  m≤n = Left (s≤s m≤n)
...| Right n≤m = Right (s≤s n≤m)

data Vec (A : Set) : ℕ → Set where
  []  :                      Vec A Z
  _∷_ : ∀{n} → A → Vec A n → Vec A (S n)

mutual
  data Vec≤ : ℕ → Set where
    nil   : Vec≤ Z
    consZ : ℕ → Vec≤ (S Z)
    consS : ∀{n} → (k : ℕ) → (v : Vec≤ (S n))
                 → k ≤ Vec≤-head v
                 → Vec≤ (S (S n))

  Vec≤-head : ∀{n} → Vec≤ (S n) → ℕ
  Vec≤-head (consZ x)     = x 
  Vec≤-head (consS x _ _) = x 

mutual
  insert : ∀{n} → ℕ → Vec≤ n → Vec≤ (S n)
  insert k nil       = consZ k
  insert k (consZ x) with compare x k
  ...| Left  k≤x = consS k (consZ x) k≤x
  ...| Right x≤k = consS x (consZ k) x≤k
  insert k (consS x xs x≤xs) with compare x k
  ...| Left  k≤x = consS k (consS x xs x≤xs) k≤x
  ...| Right x≤k = consS x (insert k xs) (insert-lemma k x xs x≤k x≤xs)

  insert-lemma : ∀{n} → (k x : ℕ)(xs : Vec≤ (S n))
               → (x ≤ k) → (x ≤ Vec≤-head xs)
               → x ≤ Vec≤-head (insert k xs)
  insert-lemma k x (consZ x₀) x≤k x≤xs with compare x₀ k
  ...| Left  _ = x≤k
  ...| Right _ = x≤xs
  insert-lemma k x (consS y ys _) x≤k x≤xs with compare y k
  ...| Left  _ = x≤k
  ...| Right _ = x≤xs

Vec-cata : ∀{A}{R : ℕ → Set}
         → (∀{k} → A → R k → R (S k))
         → R Z
         → {n : ℕ} → Vec A n → R n
Vec-cata         consF nilF []       = nilF
Vec-cata {R = R} consF nilF (x ∷ xs) = consF x (Vec-cata {R = R} consF nilF xs)

_++_ : ∀{A m n} → Vec A m → Vec A n → Vec A (m + n)
_++_ {A} {m} {n} v u = Vec-cata {R = λ k → Vec A (k + n)} _∷_ u v

insertion-sort : ∀{n} → Vec ℕ n → Vec≤ n
insertion-sort = Vec-cata (λ{k} → insert {k}) nil

v1 : Vec ℕ 4
v1 = 5 ∷ (12 ∷ (3 ∷ (8 ∷ [])))

