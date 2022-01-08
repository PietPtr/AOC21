{-# LANGUAGE RecordWildCards #-}

module Lib where
    
import Clash.Prelude as C hiding ((++), take, map)
import Prelude  hiding ((!!))
import Types

import Data.Char
import Data.List as L hiding ((!!))
import Data.Maybe

(>>>) :: Bits a => a -> Int -> a
(>>>) = shiftR

(<<<) :: Bits a => a -> Int -> a
(<<<) = shiftL

boolaToMaybe :: a -> Bool -> Maybe a
boolaToMaybe a True = Just a
boolaToMaybe _ False = Nothing

maybeToBoola :: Maybe a -> (a, Bool)
maybeToBoola (Just a) = (a, True)
maybeToBoola (Nothing) = (errorX "Strobe is false, value doesn't matter.", False)

sendBuffer :: HiddenClockResetEnable dom =>
    Signal dom (Bool, Maybe SerialByte) -> Signal dom (Maybe SerialByte)
sendBuffer = (mealy sendBufferMealy emptySendBufferState)

data SendBufferState (a :: Nat) = SendBufferState {
        memory :: Vec a SerialByte,
        writePointer :: Index a,
        readPointer :: Index a
    } deriving (Show, Generic, NFDataX)

emptySendBufferState = SendBufferState {
        memory = C.repeat 1,
        writePointer = 0,
        readPointer = 0
    }

sendBufferMealy :: (SendBufferState 32) -> (Bool, Maybe SerialByte) 
    -> (SendBufferState 32, Maybe SerialByte)
sendBufferMealy state (canSend, inData) = (state', outData)
    where
        SendBufferState {..} = state
        state' = state {
                writePointer = case inData of
                    Just _ -> writePointer + 1
                    _ -> writePointer,
                memory = case inData of
                    Just a -> replace writePointer a memory
                    _ -> memory,
                readPointer = if doSend
                    then readPointer + 1
                    else readPointer
            }
        outData = if doSend
            then Just (memory !! readPointer)
            else Nothing

        doSend = canSend && writePointer > readPointer


intertool :: [a] -> [a] -> [a]
intertool _ [] = []
intertool sep (x:xs) = x : (sep ++ (intertool sep xs))


strToSerial :: Int -> String -> [Maybe SerialByte]
strToSerial delay str = intertool (take delay $ L.repeat Nothing) (map (Just . fromIntegral . ord) str)

serialToStr :: [Maybe SerialByte] -> String
serialToStr mbytes = (map (chr . fromIntegral) $ catMaybes mbytes)