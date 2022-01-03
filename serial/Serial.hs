import Clash.Prelude
import Data.Maybe
import Debug.Trace

import qualified Data.List as L

type Btn = Bool
type Data = Unsigned 8

type State = (Unsigned 5)

data Activity = Sending | Buffering

dataOutput :: (State) -> (Bool) -> (State, (Data, Bool))
dataOutput ptr canSend = (ptr', (dataOut, strobeOut))
    where
        strobeOut = canSend
        dataOut =  alphabet !! ptr

        ptr' = trace (show ptr) $ if strobeOut
            then (ptr + 1) `mod` 26
            else ptr


dataOutputB = mealy dataOutput 0


alphabet :: Vec 26 Data
alphabet = 
    65:>66:>67:>68:>69:>70:>71:>72:>73:>74:>75:>76:>77:>
    78:>79:>80:>81:>82:>83:>84:>85:>86:>87:>88:>89:>90:>Nil


{-# ANN topEntity
  (Synthesize
    { t_name   = "serial"
    , t_inputs = [ PortName "clk"
                 , PortName "reset"
                 , PortName "enable"
                 , PortName "cansend" ]
    , t_output = PortName "out" 
    }) #-}
topEntity ::
     Clock System
     -> Reset System
     -> Enable System
     -> Signal System (Bool)
     -> Signal System (Data, Bool)
topEntity = exposeClockResetEnable dataOutputB