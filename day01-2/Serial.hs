module Serial where

import Clash.Prelude
import Data.Maybe
import Debug.Trace
import Lib
import Types
import Puzzle

import qualified Data.List as L


top :: HiddenClockResetEnable dom =>
    Signal dom (Bool, SerialByte, Bool) -> Signal dom (SerialByte, Bool)
top input = maybeToBoola <$> outWords
    where
        (canSend, dataIn, inStrobe) = unbundle input
        outWords = top' $ bundle (canSend, boolaToMaybe <$> dataIn <*> inStrobe)

top' :: HiddenClockResetEnable dom =>
    Signal dom (Bool, Maybe SerialByte) -> Signal dom (Maybe SerialByte)
top' input = sendBufferWords
    where
        (canSend, dataIn) = unbundle input

        parsed = parser dataIn
        solved = solver parsed
        serializerOut = serializer solved
        sendBufferWords = sendBuffer $ bundle (canSend, serializerOut)

{-# ANN topEntity
  (Synthesize
    { t_name   = "serial"
    , t_inputs = [ PortName "clk"
                 , PortName "reset"
                 , PortName "enable"
                 , PortProduct "" [PortName "can_send", PortName "data_in", PortName "in_stb"] ]
    , t_output = PortProduct "" [PortName "data_out", PortName "out_stb"]
    }) #-}
topEntity ::
     Clock System
     -> Reset System
     -> Enable System
     -> Signal System (Bool, SerialByte, Bool)
     -> Signal System (SerialByte, Bool)
topEntity = exposeClockResetEnable top