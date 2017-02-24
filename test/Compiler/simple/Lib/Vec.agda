module Lib.Vec where

open import Common.Nat renaming (zero to Z; suc to S)
open import Lib.Fin
open import Common.Unit
import Common.List -- using (List ; [] ; _∷_)

data Vec (A : Set) :  Nat -> Set where
  _∷_ : forall {n} -> A -> Vec A n -> Vec A (S n)
  []   : Vec A Z

infixr 30 _++_

_++_ : {A : Set}{m n : Nat} -> Vec A m -> Vec A n -> Vec A (m + n)
[]        ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

snoc : {A : Set}{n : Nat} -> Vec A n -> A -> Vec A (S n)
snoc []        e = e ∷ []
snoc (x ∷ xs) e = x ∷ snoc xs e

length : {A : Set}{n : Nat} -> Vec A n -> Nat
length [] = Z
length (x ∷ xs) = 1 + length xs

length' : {A : Set}{n : Nat} -> Vec A n -> Nat
length' {n = n} _ = n

zipWith3 : ∀ {A B C D n} -> (A -> B -> C -> D) -> Vec A n -> Vec B n -> Vec C n -> Vec D n
zipWith3 f []        []        []        = []
zipWith3 f (x ∷ xs) (y ∷ ys) (z ∷ zs) = f x y z ∷ zipWith3 f xs ys zs

zipWith : ∀ {A B C n} -> (A -> B -> C) -> Vec A n -> Vec B n -> {u : Unit} -> Vec C n
zipWith _ [] [] = []
zipWith f (x ∷ xs) (y ∷ ys) = f x y ∷ zipWith f xs ys {u = unit}

_!_ : ∀ {A n} -> Vec A n -> Fin n -> A
(x ∷ xs) ! fz = x
(_ ∷ xs) ! fs n = xs ! n
[] ! ()

_[_]=_ : {A : Set}{n : Nat} -> Vec A n -> Fin n -> A -> Vec A n
(a ∷ as) [ fz ]= e = e ∷ as
(a ∷ as) [ fs n ]= e = a ∷ (as [ n ]= e)
[] [ () ]= e

map : ∀ {A B n}(f : A -> B)(xs : Vec A n) -> Vec B n
map f []        = []
map f (x ∷ xs) = f x ∷ map f xs

forgetL : {A : Set}{n : Nat} -> Vec A n -> Common.List.List A
forgetL [] = Common.List.[]
forgetL (x ∷ xs) = x Common.List.∷ forgetL xs

rem : {A : Set}(xs : Common.List.List A) -> Vec A (Common.List.length xs)
rem Common.List.[] = []
rem (x Common.List.∷ xs) = x ∷ rem xs
