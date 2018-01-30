module SimIt.SystemVerilog.Logic.TwoStateLogicTest

import Data.Vect
import SimIt.SystemVerilog.Logic.LogicVector
import SimIt.SystemVerilog.Logic.TwoStateLogic
import SimIt.SystemVerilog.Logic.Types
import Specdris.Spec

testCase : IO ()
testCase
  = do spec $ do
        describe "SimIt.Verilog.Logic.TwoStateLogic" $ do
          describe "TwoStateLogicVector" $ do
            let lsb2 = the (TwoStateLogicVector 2) (LvLsb 1 0 [Logic0, Logic1])
            let msb2 = the (TwoStateLogicVector 2) (LvMsb 0 1 [Logic0, Logic1])
            let lsb8 = the (TwoStateLogicVectorS 8) $ LvLsb 15 8 [Logic1, Logic0, Logic0, Logic1, Logic0, Logic1, Logic1, Logic0]
            let msb8 = the (TwoStateLogicVectorS 8) $ LvMsb 8 15 [Logic1, Logic0, Logic0, Logic1, Logic0, Logic1, Logic1, Logic0]

            describe "#getAt" $ do
              it "gets the two state logic value at the given index" $ do
                getAt lsb2 (LsbIndex 0) === Logic1
                getAt msb2 (MsbIndex 0) === Logic0
                getAt lsb8 (LsbIndex 12) === Logic1
                getAt msb8 (MsbIndex 12) === Logic0
            
            describe "#setAt" $ do
              it "sets the logic value at the given index" $ do
                let lsb2' = setAt lsb2 (LsbIndex 0) Logic0
                getAt lsb2' (LsbIndex 0) === Logic0

export  
specSuite : IO ()
specSuite = do putStrLn "\n  spec:"
               testCase