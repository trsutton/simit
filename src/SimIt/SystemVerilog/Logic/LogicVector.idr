module SimIt.SystemVerilog.Logic.LogicVector

import Prelude
import Data.Fin
import Data.Vect
import SimIt.SystemVerilog.Logic.Types

%access public export
%default total

||| A non-empty vector of logic values with an explicit signedness and length
||| @ signedness the signedness of the vector
||| @ len the length of the vector
data LogicVector : (signedness : Signedness) -> (len : Nat) -> Type -> Type where
    ||| A logic vector that uses LSB-n bit numbering (e.g. logic [7:0])
    LvLsb : LogicValue a =>
            (msb : Nat) ->
            (lsb : Nat) ->
            {auto bitNumberingLsb : LTE lsb msb} ->
            Vect (S (msb - lsb)) a ->
            LogicVector signedness (S (msb - lsb)) a

    ||| A logic vector that uses MSB-n bit numbering (e.g. logic [0:7])
    LvMsb : LogicValue a =>
            (msb : Nat) ->
            (lsb : Nat) ->
            {auto bitNumberingMsb : LTE msb lsb} ->
            Vect (S (lsb - msb)) a ->
            LogicVector signedness (S (lsb - msb)) a

%name LogicVector xs,ys,zs,ws
            
-------------------------------------------------------------------------------
-- Length
-------------------------------------------------------------------------------

||| Computes the length of a `LogicVector`.
length : (xs : LogicVector s n a) -> Nat
length (LvLsb msb lsb xs) = S (msb - lsb)
length (LvMsb msb lsb xs) = S (lsb - msb)

||| Proves that `length` computes the length of a `LogicVector` correctly
lengthComputesCorrectly : (len : Nat) -> (xs : LogicVector s len a) -> (length xs = len)
lengthComputesCorrectly Z xs impossible
lengthComputesCorrectly (S (msb - lsb)) (LvLsb msb lsb _) = Refl
lengthComputesCorrectly (S (lsb - msb)) (LvMsb msb lsb _) = Refl

-- -------------------------------------------------------------------------------
-- -- Indexing logic vectors
-- -------------------------------------------------------------------------------

||| Index within the bounds of a `LogicVector`.
||| @ n the index
||| @ xs the logic vector
data Index : (n : Nat) -> (xs : LogicVector s len a) -> Type where
    ||| An in-bound index for a `LogicVector` that uses LSB-n bit numbering
    LsbIndex : LogicValue a =>
               (n : Nat) -> 
               {auto bitNumberingLsb : LTE lsb msb} ->
               {auto lteMsb : LTE n msb} -> 
               {auto gteLsb : GTE n lsb} -> 
               {auto xs : Vect (S (msb - lsb)) a} -> 
               Index n (LvLsb msb lsb xs)

    ||| An in-bound index for a `LogicVector` that uses MSB-n bit numbering
    MsbIndex : LogicValue a =>
               (n : Nat) ->
               {auto bitNumberingMsb : LTE msb lsb} ->
               {auto gteMsb : GTE n msb} ->
               {auto lteLsb : LTE n lsb} ->
               {auto xs : Vect (S (lsb - msb)) a} ->
               Index n (LvMsb msb lsb xs)

||| Range (i.e. part select) within the bounds of a `LogicVector`
||| @ sz the size of the `Range`
||| @ lv the `LogicVector` to which the range applies
||| @ m the inclusive index of the start of the range/slice
||| @ n the inclusive index of the end of the range/slice
data Range : {sz : Nat} -> (lv : LogicVector s len a) -> (m : Nat) -> (n : Nat) -> Type where
    ||| An in-bound range for a `LogicVector` that uses LSB-n bit numbering
    LsbRange : LogicValue a =>
               (m : Nat) ->
               (n : Nat) ->
               {auto bitNumberingLsb : LTE lsb msb} ->
               {auto mLteMsb : LTE m msb} -> 
               {auto mGteLsb : GTE m lsb} ->
               {auto nLteMsb : LTE n msb} ->
               {auto nGteLsb : GTE n lsb} ->
               {auto xs : Vect (S (msb - lsb)) a} ->    
               {auto validIndices : LTE n m} ->
               Range {sz = S (m - n)} (LvLsb msb lsb xs) m n
    
    ||| An in-bound range for a `LogicVector` tat uses MSB-n bit numbering
    MsbRange : LogicValue a =>
               (m : Nat) ->
               (n : Nat) ->
               {auto bitNumberingMsb : LTE msb lsb} ->
               {auto mGteMsb : GTE m msb} ->
               {auto mLteLsb : LTE m lsb} ->
               {auto nGteMsb : GTE n msb} ->
               {auto nLteLsb : LTE n lsb} ->
               {auto xs : Vect (S (lsb - msb)) a} ->
               {auto validIndices : LTE m n} ->
               Range {sz = S (n - m)} (LvMsb msb lsb xs) m n

||| Creates a `Fin` that bounds the given integer
restrict' : (n : Nat) -> Integer -> Fin (S n)
restrict' n val = let val' = assert_total (abs (mod val (cast (S n)))) in
                    fromInteger {n = S n} val' 
                        {prf = believe_me {a=IsJust (Just val')} ItIsJust}

||| Get the element at the given index of a non-empty vector
getAt : (lv : LogicVector s len a) -> Index n lv -> a
getAt (LvLsb msb lsb xs) (LsbIndex n) = 
    let idx = restrict' (msb - lsb) (toIntegerNat (msb - n)) 
    in Vect.index idx xs

getAt (LvMsb msb lsb xs) (MsbIndex n) = 
    let idx = restrict' (lsb - msb) (toIntegerNat (n - msb)) 
    in Vect.index idx xs

||| Set the element at the given index of a non-empty vector
setAt : (lv : LogicVector s len a) -> Index n lv -> a -> LogicVector s len a
setAt (LvLsb msb lsb xs) (LsbIndex n) x = 
    let idx = restrict' (msb - lsb) (toIntegerNat (msb - n))
        xs' = Vect.replaceAt idx x xs 
    in LvLsb msb lsb xs'
        
setAt (LvMsb msb lsb xs) (MsbIndex n) x = 
    let idx = restrict' (lsb - msb) (toIntegerNat (n - msb))
        xs' = Vect.replaceAt idx x xs 
    in LvMsb msb lsb xs'

||| Selects a range of bits from a `LogicVector`
select : (lv : LogicVector s len a) -> Range {sz} lv m n -> LogicVector s sz a
select (LvLsb _ _ _) (MsbRange _ _) impossible
select (LvMsb _ _ _) (LsbRange _ _) impossible
select (LvLsb msb lsb xs) (LsbRange m n) = 
    let nDrop = msb - n
        nTake = S (m - n)
        xs' = (Vect.take nTake . Vect.drop nDrop) xs
    in LvLsb m n xs'
select (LvMsb msb lsb xs) (MsbRange m n) = ?select_rhs_msb
