module Puzzle where

import Clash.Prelude

import Types
import Lib
import Data.Char
import Data.Maybe

data Direcion = DForward | DUp | DDown
    deriving (Show, Generic, NFDataX)


(<<$>>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<<$>>) = (fmap . fmap)

parser :: HiddenClockResetEnable dom =>
    Signal dom (Maybe SerialByte) -> Signal dom (Maybe PuzzleInput)
parser mbytes = result
    where
        -- wires
        enableParser = toEnable $ isJust <$> mbytes
        result = parserM enableParser (bullshitFromJust <$> mbytes)

        -- defs
        bullshitFromJust maybe = case maybe of
            Just a -> a
            Nothing -> 0x07 -- Bell in ASCII :P

        parserM = exposeEnable (mealy justParse Newline)

data ParseState 
    = Newline 
    | WaitForward (Index 7) 
    | WaitDown (Index 4) 
    | WaitUp (Index 2) 
    | ReadNum Direcion
    deriving (Show, Generic, NFDataX)

justParse :: ParseState -> SerialByte -> (ParseState, Maybe PuzzleInput)
justParse state inp = (state', output )
    where
        next state ctr dir = if ctr == 0
            then ReadNum dir
            else state

        inpChar = chr $ fromIntegral inp
        
        state' = case state of
            Newline -> case inpChar of
                'f' -> WaitForward maxBound
                'd' -> WaitDown maxBound
                'u' -> WaitUp maxBound
            WaitForward ctr -> next (WaitForward $ ctr - 1) ctr DForward
            WaitDown ctr -> next (WaitDown $ ctr - 1) ctr DDown
            WaitUp ctr -> next (WaitUp $ ctr - 1) ctr DUp
            ReadNum _ -> Newline

        output = case state of
            ReadNum dir -> case dir of
                DForward -> Just $ Forward inp'
                DDown -> Just $ Down inp'
                DUp -> Just $ Up inp'
            _ -> Nothing
            where
                inp' = fromIntegral inp



solver :: HiddenClockResetEnable dom =>
    Signal dom (Maybe PuzzleInput) -> Signal dom (Maybe PuzzleOutput)
solver inp = pure Nothing

serializer :: HiddenClockResetEnable dom =>
    Signal dom (Maybe PuzzleOutput) -> Signal dom (Maybe SerialByte)
serializer outp = pure Nothing


