{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module HW08 where

import Prelude (Show(..), Eq(..), ($), (.), flip)


-- Propositional Logic --------------------------------

-- False, the uninhabited type
data False

-- Logical Not
type Not p = p -> False

-- Logical Disjunction
data p \/ q = Left  p
            | Right q

-- Logical Conjunction
data p /\ q = Conj p q

-- If and only if
type p <-> q = (p -> q) /\ (q -> p)

-- Admit is used to assume an axiom without proof
admit :: p
admit = admit

-- There is no way to prove this axiom in constructive logic, therefore we
-- leave it admitted
excluded_middle :: p \/ Not p
excluded_middle = admit

absurd :: False -> p
absurd false = admit

double_negation :: p <-> Not (Not p)
double_negation = Conj (\p not_p -> not_p p) admit

modus_ponens :: (p -> q) -> p -> q
modus_ponens = ($)

modus_tollens :: (p -> q) -> Not q -> Not p
modus_tollens = flip (.)

material_implication :: (p -> q) <-> (Not p \/ q)
-- The proof has two parts, the forward direction (->) and
--   the backwards direction (<-)
material_implication = Conj dir1 dir2
    where 
      -- Case 1: (P -> Q) -> (~P \/ Q)
      dir1 p_imp_q =
          -- There are 2 cases, P and ~P
          case excluded_middle of
            -- SCase 1: P, then Q since P -> Q
            Left  p     -> Right $ p_imp_q p
            -- SCase 2: ~P, then ~P
            Right not_p -> Left not_p
      -- Case 2: (~P \/ Q) -> (P -> Q)
      -- SCase 1: ~P -> (P -> Q)
      dir2 (Left not_p) p =
          -- This is a contradiction since we have both
          -- P and ~P
          absurd $ not_p p
      -- SCase 2: Q -> (P -> Q)
      dir2 (Right q)    _ =
          -- q is a witness for the proposition Q,
          -- therefore we can just return it
          q

-- Exercise 1 -----------------------------------------

disjunctive_syllogism :: (p \/ q) -> Not p -> q
disjunctive_syllogism = admit

-- Exercise 2 -----------------------------------------

composition :: (p -> q) \/ (p -> r) -> p -> q \/ r
composition = admit

-- Exercise 3 -----------------------------------------

transposition :: (p -> q) <-> (Not q -> Not p)
transposition = admit

-- Exercise 4 -----------------------------------------

de_morgan :: Not (p \/ q) <-> (Not p /\ Not q)
de_morgan = admit

-- Natural Numbers ------------------------------------

data Nat = O | S Nat
           deriving (Show, Eq)

type family (n :: Nat) + (m :: Nat) :: Nat
type instance O     + m = m
type instance (S n) + m = S (n + m)
infixl 6 +

data Forall n where
    Zero :: Forall O
    Succ :: Forall n -> Forall (S n)

data (n :: Nat) == (m :: Nat) where
    Refl :: n == n
infix 4 ==

type (n :: Nat) /= (m :: Nat) = Not (n == m)
infix 4 /=

data n < m where
  LT_Base :: O < S n
  LT_Rec  :: n < m -> S n < S m

type n >  m = m < n
type n <= m = (n < m) \/ (n == m)
type n >= m = m <= n

-- Weakening Lemma
neq_weaken :: S n /= S m -> n /= m
neq_weaken h_neq Refl = h_neq Refl

{- ********************************************************
   * Theorem: Not Equal Implies Greater Than or Less Then *
   ******************************************************** -}
neq_gt_lt :: Forall n -> Forall m ->
             n /= m -> (n < m) \/ (n > m)
-- The proof is by induction on n and m
-- Base Case 1: both n and m are 0. This is impossible since the hypothesis h
--   states that n /= m
neq_gt_lt Zero  Zero        h = absurd $ h Refl
-- Base Case 2: n == 0 and m > 0. Here we choose the left case, n < m
neq_gt_lt Zero (Succ m)     _ = Left  LT_Base
-- Base Case 3: n > 0 and m == 0. Here we choose the right case, n > m
neq_gt_lt (Succ n) Zero     _ = Right LT_Base
-- Inductive Step: both n and m are greater than 0
neq_gt_lt (Succ n) (Succ m) h_neq =
    -- We generate an induction hypothesis by invoking a recursive call on n,
    -- m, and the weakening hypothesis
    case neq_gt_lt n m (neq_weaken h_neq) of
      -- Case 1: n < m with witness w. This means that S n < S m
      Left  w -> Left  $ LT_Rec w
      -- Case 2: n > m with witness w. This means that S n > S m
      Right w -> Right $ LT_Rec w

-- Exercise 5 -----------------------------------------

o_plus_n :: O + n == n
o_plus_n = Refl

n_plus_0 :: Forall n -> n + O == n
-- Base Case:
n_plus_0  Zero    = Refl {- :: O + O == O -}
-- Inductive Step:
n_plus_0 (Succ n) = case n_plus_0 n of
                      Refl   {- :: n   + O == n   -} -> 
                        Refl {- :: S n + O == S n -}

add_zero :: Forall n -> O + n == n + O
add_zero = admit

-- Exercise 6 -----------------------------------------

n_lt_sn :: Forall n -> n < S n
n_lt_sn = admit

-- Exercise 7 -----------------------------------------

data Even :: Nat -> * where
    E_Zero :: Even O
    E_Rec  :: Even n -> Even (S (S n))

data Odd :: Nat -> * where
    O_One :: Odd (S O)
    O_Rec :: Odd n -> Odd (S (S n))

even_plus_one :: Even n -> Odd (S n)
-- Base Case: The successor of zero is odd
even_plus_one  E_Zero   = O_One
-- Inductive Step: if S (S n) is even then S (S (S n)) is odd
even_plus_one (E_Rec n) = O_Rec $ even_plus_one n

odd_plus_one :: Odd n -> Even (S n)
odd_plus_one = admit

-- Exercise 8 -----------------------------------------

succ_sum :: Forall n -> Forall m ->
            S n + S m == S (S (n + m))
succ_sum  Zero    m = Refl
succ_sum (Succ n) m = case succ_sum n m of
                        Refl -> Refl

double_even :: Forall n -> Even (n + n)
double_even = admit
