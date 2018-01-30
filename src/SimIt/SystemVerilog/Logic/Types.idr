module SimIt.SystemVerilog.Logic.Types

%access public export
%default total

||| Represents the signedness of an integral value
data Signedness
    = Signed
    | Unsigned

||| Interface for logic values. Provides support for bitwise operations
interface LogicValue a where
    ||| Computes the bitwise NOT of a `LogicValue`.
    bnot  : a -> a
    ||| Computes the bitwise AND of two `LogicValue` values.
    band  : a -> a -> a
    ||| Computes the bitwise NAND of two `LogicValue` values.
    bnand : a -> a -> a
    ||| Computes the bitwise OR of two `LogicValue` values.
    bor   : a -> a -> a
    ||| Computes the bitwise NOR of two `LogicValue` values.
    bnor  : a -> a -> a
    ||| Computes the bitwise XOR of two `LogicValue` values.
    bxor  : a -> a -> a
    ||| Computes the bitwise XNOR of two `LogicValue` values.
    bxnor : a -> a -> a

    bnand x y = bnot $ band x y
    bnor x y  = bnot $ bor x y
    bxnor x y = bnot $ bxor x y