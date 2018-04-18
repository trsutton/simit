module SimIt.SystemVerilog.Logic.BoundVect

import Prelude
import Data.Vect

%access public export
%default total


takeN : (n : Nat) -> {auto prf: LTE n m} -> Vect m a -> Vect n a
takeN Z xs {prf = LTEZero} = []
takeN (S left) (y :: xs) {prf = (LTESucc x)} = y :: takeN left xs


cancelZeros : (left, right : Nat) -> (S right = Z + minus left Z + minus (S right) left)
  -> (S right = left + minus (S right) left)
cancelZeros Z Z Refl = Refl
cancelZeros Z (S k) Refl = Refl
cancelZeros (S k) Z prf = prf
cancelZeros (S k) (S j) prf = prf


droppable : LTE k j -> LTE k i -> LTE j i -> (S i) = k + ((minus j k) + (minus (S i) j))
droppable LTEZero LTEZero LTEZero = Refl
droppable LTEZero LTEZero (LTESucc {right} {left} x) =
  eqSucc (S right) (plus left (minus (S right) left)) (cancelZeros left right (droppable LTEZero LTEZero x))
droppable (LTESucc x) (LTESucc y) (LTESucc z) = let rec = (droppable x y z) in
  rewrite rec in Refl


cancelPlus : (left, right : Nat) -> (LTE left right) -> right = plus left (minus right left)
cancelPlus Z Z LTEZero = Refl
cancelPlus Z (S k) LTEZero = Refl
cancelPlus (S k) (S j) (LTESucc x) = eqSucc _ _ (cancelPlus k j x)


cancelPlusPlus : (left, right, left1 : Nat) -> (LTE left right) -> (LTE left left1) -> LTE left1 right ->
  right = (plus left (plus (minus left1 left) (minus right left1)))
cancelPlusPlus Z Z Z LTEZero LTEZero LTEZero = Refl
cancelPlusPlus Z (S k) Z LTEZero LTEZero LTEZero = Refl
cancelPlusPlus Z (S k) (S j) LTEZero LTEZero (LTESucc z) = eqSucc _ _ (cancelPlus j k z)
cancelPlusPlus (S i) (S k) (S j) (LTESucc x) (LTESucc y) (LTESucc z) = eqSucc _ _ (cancelPlusPlus i k j x y z)


droppable2 : (x : LTE i k) -> (y : LTE k n) -> (z : LTE j n) -> (w : LTE i j) -> (s : LTE k j) -> (t : LTE i n) -> S (minus n i) = (minus k i) + ((minus j k) + (minus (S n) j))
droppable2 {n} LTEZero LTEZero LTEZero LTEZero LTEZero LTEZero = rewrite (minusZeroRight n) in Refl
droppable2 LTEZero LTEZero (LTESucc x {left} {right}) LTEZero LTEZero LTEZero
  = eqSucc _ _ (cancelPlus _ _ (lteSuccRight x))
droppable2 LTEZero (LTESucc {left} {right} x) (LTESucc {left=left1} y) LTEZero (LTESucc z) LTEZero = eqSucc _ _ (cancelPlusPlus left (S right) left1 (lteSuccRight x) z (lteSuccRight y))
droppable2 (LTESucc x) (LTESucc y) (LTESucc z) (LTESucc w) (LTESucc s) (LTESucc t) =
  let rec = (droppable2 x y z w s t) in rewrite rec in Refl


makeDroppable : i = j -> Vect i a -> Vect j a
makeDroppable Refl xs = xs


cancelPlusLte : (x : LTE left right) -> LTE (S left) (plus left (minus (S right) left))
cancelPlusLte {left = Z} {right = Z} LTEZero = LTESucc LTEZero
cancelPlusLte {left = Z} {right = (S k)} LTEZero = LTESucc LTEZero
cancelPlusLte {left = (S k)} {right = (S j)} (LTESucc x) = LTESucc (cancelPlusLte x)

 
minusSmallEvenStill : (x : LTE k i) -> (y : LTE j i) -> (z : LTE k j) -> LTE (S (minus j k)) ((minus j k) + (minus (S i) j))
minusSmallEvenStill LTEZero LTEZero LTEZero = LTESucc LTEZero
minusSmallEvenStill LTEZero (LTESucc {right = Z} {left = Z} x) LTEZero = LTESucc (LTESucc x)
minusSmallEvenStill LTEZero (LTESucc {right = (S k)} {left = Z} x) LTEZero = LTESucc (LTESucc x)
minusSmallEvenStill LTEZero (LTESucc {right = (S k)} {left = (S j)} x) LTEZero = LTESucc (minusSmallEvenStill LTEZero x LTEZero)
minusSmallEvenStill (LTESucc x) (LTESucc y) (LTESucc z) = minusSmallEvenStill x y z


minusSmallDefinitely : (x : LTE i k) -> (y : LTE k n) -> (z : LTE j n) -> (w : LTE i j)
  -> (s : LTE k j) -> (t : LTE i n) -> LTE (S (minus j k)) ((minus j k) + (minus (S n) j))
minusSmallDefinitely LTEZero LTEZero LTEZero LTEZero LTEZero LTEZero = LTESucc LTEZero
minusSmallDefinitely LTEZero LTEZero (LTESucc {left} {right} x) LTEZero LTEZero LTEZero
  = LTESucc (cancelPlusLte x)
minusSmallDefinitely LTEZero (LTESucc x) (LTESucc y) LTEZero (LTESucc s) LTEZero
  = minusSmallEvenStill x y s
minusSmallDefinitely (LTESucc x) (LTESucc y) (LTESucc z) (LTESucc w) (LTESucc s) (LTESucc t) = minusSmallDefinitely x y z w s t


boundVect :  (lowIndex, highIndex, lowBound, highBound : Nat) ->
             (lowIndexBigger : GTE lowIndex lowBound) ->
             (lowIndexSmaller : LTE lowIndex highBound) ->
             (highIndexSmaller : LTE highIndex highBound) ->
             (highIndexBigger : GTE highIndex lowBound) ->
             (smallerIndex : LTE lowIndex highIndex) ->
             (smallerBound : LTE lowBound highBound) ->
             Vect (S (highBound - lowBound)) a -> Vect (S (highIndex - lowIndex)) a

boundVect Z Z Z Z LTEZero LTEZero LTEZero LTEZero LTEZero LTEZero xs = xs
boundVect Z Z Z (S k) LTEZero LTEZero LTEZero LTEZero LTEZero LTEZero (x :: xs) = [x]
boundVect Z (S k) Z (S j) LTEZero LTEZero x LTEZero LTEZero LTEZero (y :: xs) = y :: takeN {prf = x} (S k) xs
boundVect (S k) (S j) Z (S i) LTEZero (LTESucc x) (LTESucc y) LTEZero (LTESucc z) LTEZero (w :: xs) = takeN {prf=minusSmallEvenStill x y z} (S (minus j k)) (drop k (makeDroppable (droppable z x y) xs))
boundVect (S k) (S j) (S i) (S n) (LTESucc x) (LTESucc y) (LTESucc z) (LTESucc w) (LTESucc s) (LTESucc t) (u :: xs) =
  takeN {prf=minusSmallDefinitely x y z w s t} (S (minus j k))
        (drop {m=(minus j k) + minus (S n) j} (minus k i) (makeDroppable (droppable2 x y z w s t) (u :: xs)))


boundVectAuto : (lowIndex, highIndex, lowBound, highBound : Nat) ->
                {auto lowIndexBigger : GTE lowIndex lowBound} ->
                {auto lowIndexSmaller : LTE lowIndex highBound} ->
                {auto highIndexSmaller : LTE highIndex highBound} ->
                {auto highIndexBigger : GTE highIndex lowBound} ->
                {auto smallerIndex : LTE lowIndex highIndex} ->
                {auto smallerBound : LTE lowBound highBound} ->
                Vect (S (highBound - lowBound)) a -> Vect (S (highIndex - lowIndex)) a
boundVectAuto = boundVect


boundVectLsb :  (highIndex, lowIndex, highBound, lowBound : Nat) ->
                (lowIndexBigger : GTE lowIndex lowBound) ->
                (lowIndexSmaller : LTE lowIndex highBound) ->
                (highIndexSmaller : LTE highIndex highBound) ->
                (highIndexBigger : GTE highIndex lowBound) ->
                (smallerIndex : LTE lowIndex highIndex) ->
                (smallerBound : LTE lowBound highBound) ->
                Vect (S (highBound - lowBound)) a -> Vect (S (highIndex - lowIndex)) a
boundVectLsb highIndex lowIndex highBound lowBound lowIndexBigger lowIndexSmaller highIndexSmaller highIndexBigger smallerIndex smallerBound xs =
  boundVect lowIndex highIndex lowBound highBound lowIndexBigger lowIndexSmaller highIndexSmaller highIndexBigger smallerIndex smallerBound xs

boundVectLsbAuto : (highIndex, lowIndex, highBound, lowBound : Nat) ->
                   {auto lowIndexBigger : GTE lowIndex lowBound} ->
                   {auto lowIndexSmaller : LTE lowIndex highBound} ->
                   {auto highIndexSmaller : LTE highIndex highBound} ->
                   {auto highIndexBigger : GTE highIndex lowBound} ->
                   {auto smallerIndex : LTE lowIndex highIndex} ->
                   {auto smallerBound : LTE lowBound highBound} ->
                   Vect (S (highBound - lowBound)) a -> Vect (S (highIndex - lowIndex)) a
boundVectLsbAuto = boundVectLsb
