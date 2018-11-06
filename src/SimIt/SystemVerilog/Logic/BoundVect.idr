module SimIt.SystemVerilog.Logic.BoundVect

import Prelude
import Data.Vect

%access public export
%default total

takeN : (n : Nat) -> {auto prf: LTE n m} -> Vect m a -> Vect n a
takeN Z xs {prf = LTEZero} = []
takeN (S left) (y :: xs) {prf = (LTESucc x)} = y :: takeN left xs

lteMinusLeft : LTE k i -> LTE j i -> LTE (minus j k) i
lteMinusLeft _ LTEZero = LTEZero
lteMinusLeft LTEZero (LTESucc x) = LTESucc x 
lteMinusLeft (LTESucc x) (LTESucc y) = lteSuccRight $ lteMinusLeft x y

minusBounded : GTE k i -> 
               LTE k n -> 
               LTE j n -> 
               GTE j i -> 
               LTE k j -> 
               LTE i n -> LTE (minus j k) (minus n i)
minusBounded _ _ LTEZero _ _ _ = LTEZero
minusBounded _ LTEZero (LTESucc y) _ _ LTEZero = LTESucc y
minusBounded _ (LTESucc y) (LTESucc z) _ _ LTEZero = lteSuccRight $ lteMinusLeft y z
minusBounded (LTESucc x) (LTESucc y) (LTESucc z) (LTESucc w) (LTESucc s) (LTESucc t) 
  = minusBounded x y z w s t

boundVect : (lowIndex, highIndex, lowBound, highBound : Nat) ->
            {auto lowIndexBigger : GTE lowIndex lowBound} ->
            {auto lowIndexSmaller : LTE lowIndex highBound} ->
            {auto highIndexSmaller : LTE highIndex highBound} ->
            {auto highIndexBigger : GTE highIndex lowBound} ->
            {auto smallerIndex : LTE lowIndex highIndex} ->
            {auto smallerBound : LTE lowBound highBound} ->
            Vect (S (highBound - lowBound)) a -> Vect (S (highIndex - lowIndex)) a 
boundVect _ Z _ _ (x :: xs) = [x]
boundVect Z (S k) Z (S j) (x :: xs) = x :: takeN (S k) xs 
boundVect (S k) (S j) Z _
    {lowIndexSmaller = (LTESucc y)}
    {highIndexSmaller = (LTESucc z)} (x :: xs) 
  = x :: takeN (minus j k) {prf=lteSuccRight $ lteMinusLeft y z} xs
boundVect (S k) (S j) (S i) (S n) (x :: xs) 
    {lowIndexBigger = (LTESucc y1)}
    {lowIndexSmaller = (LTESucc y2)}
    {highIndexSmaller = (LTESucc y3)}
    {highIndexBigger = (LTESucc y4)}
    {smallerIndex = (LTESucc y5)}
    {smallerBound = (LTESucc y6)} 
  = x :: takeN (minus j k) {prf=minusBounded y1 y2 y3 y4 y5 y6} xs

boundVectLsb : (highIndex, lowIndex, highBound, lowBound : Nat) ->
                   {auto lowIndexBigger : GTE lowIndex lowBound} ->
                   {auto lowIndexSmaller : LTE lowIndex highBound} ->
                   {auto highIndexSmaller : LTE highIndex highBound} ->
                   {auto highIndexBigger : GTE highIndex lowBound} ->
                   {auto smallerIndex : LTE lowIndex highIndex} ->
                   {auto smallerBound : LTE lowBound highBound} ->
                   Vect (S (highBound - lowBound)) a -> Vect (S (highIndex - lowIndex)) a
boundVectLsb highIndex lowIndex highBound lowBound xs 
  = boundVect lowIndex highIndex lowBound highBound xs
