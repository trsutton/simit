module SimIt.SystemVerilog.Logic.TwoStateLogic

import Data.Vect
import SimIt.SystemVerilog.Logic.LogicVector
import SimIt.SystemVerilog.Logic.Types

%access public export
%default total

data TwoStateLogicValue : Type where
    Logic0 : TwoStateLogicValue
    Logic1 : TwoStateLogicValue

TwoStateLogicVector : Nat -> Type
TwoStateLogicVector len = LogicVector Unsigned len TwoStateLogicValue

TwoStateLogicVectorS : Nat -> Type
TwoStateLogicVectorS len = LogicVector Signed len TwoStateLogicValue

implementation LogicValue TwoStateLogicValue where
    bnot Logic0 = Logic1
    bnot Logic1 = Logic0

    band Logic1 Logic1 = Logic1
    band _ _ = Logic0
    
    bor Logic1 _ = Logic1
    bor _ Logic1 = Logic1
    bor _ _ = Logic0

    bxor Logic0 Logic1 = Logic1
    bxor Logic1 Logic0 = Logic1
    bxor _ _ = Logic0

implementation Show TwoStateLogicValue where
    show Logic0 = "0"
    show Logic1 = "1"

implementation Eq TwoStateLogicValue where
    Logic0 == Logic0 = True
    Logic1 == Logic1 = True
    _ == _ = False

namespace Examples
    xs1 : TwoStateLogicVector 1
    xs1 = LvLsb 0 0 [Logic0]

    xs2 : TwoStateLogicVector 2
    xs2 = LvLsb 1 0 [Logic0, Logic1]

    xs3 : TwoStateLogicVector 3
    xs3 = LvLsb 7 5 [Logic1, Logic0, Logic1]

    xs8 : TwoStateLogicVector 8
    xs8 = LvMsb 8 15 [Logic0, Logic1, Logic0, Logic1, Logic1, Logic0, Logic1, Logic0]