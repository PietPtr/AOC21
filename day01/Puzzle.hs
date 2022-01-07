{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NumericUnderscores #-}
module Puzzle where

import Clash.Prelude

import Types
import Lib
import Debug.Trace

parser :: HiddenClockResetEnable dom =>
    Signal dom (Maybe SerialByte) -> Signal dom (Maybe PuzzleInput)
parser = mealy parserMealy emptyParserState

solver :: HiddenClockResetEnable dom =>
    Signal dom (Maybe PuzzleInput) -> Signal dom (Maybe PuzzleOutput)
solver = mealy solverMealy emptySolverState

serializer :: HiddenClockResetEnable dom =>
    Signal dom (Maybe PuzzleOutput) -> Signal dom (Maybe SerialByte)
serializer = mealy serializerMealy emptySerializerState

data ParserState = ParserState {
        digitIdx :: Index 4,
        numbers :: Vec 4 (Unsigned 14),
        done :: Bool
    } deriving (Show, Generic, NFDataX)

emptyParserState = ParserState {
        digitIdx = 0,
        numbers = 0:>0:>0:>0:>Nil,
        done = False
    }

data Action = OutputNumber | ReadNextDigit | End | Stall
    deriving Show

parserMealy :: ParserState -> (Maybe SerialByte) -> (ParserState, Maybe PuzzleInput)
parserMealy state input = (state', output)
    where
        ParserState {..} = state
        msb = (\a -> resize (a >>> 4)) <$> input :: Maybe (BitVector 4)

        action = case msb of
            Just 0b0011 -> ReadNextDigit
            Just 0b0000 -> OutputNumber
            Nothing -> Stall
            _ -> End -- invalid input i guess

        done' = (digitIdx == 0 && input == Just 0x0a) || done

        numbers' = case action of
            OutputNumber -> repeat 0
            _ -> case input of
                Just inDigit -> 
                    replace digitIdx (fromIntegral (inDigit .&. 0b1111)) 
                    (map (*10) numbers)
                Nothing -> numbers

        digitIdx' = case action of
            OutputNumber -> 0
            ReadNextDigit -> if digitIdx < 3
                then digitIdx + 1
                else digitIdx
            End -> 0
            Stall -> digitIdx
        
        output = case action of
            OutputNumber -> if done'
                then Just EOF
                else Just $ Height $ sum (map resize numbers)
            _ -> Nothing -- TODO ?

        state' = ParserState {
                digitIdx = digitIdx',
                numbers = numbers',
                done = done'
            }



type SolverState = (Unsigned 32, SolverPhase)
emptySolverState = (0, NoInput maxBound)

data SolverPhase 
    = NoInput (Unsigned 32)
    | InputReady (Unsigned 32) (Unsigned 32) 
    | ResultReady
    | Finished 
    deriving (Show, Generic, NFDataX)

solverMealy :: SolverState -> Maybe PuzzleInput -> (SolverState, Maybe PuzzleOutput)
solverMealy (count, phase) puzzleIn = (state', output)
    where
        phase' = case phase of
            Finished -> Finished
            ResultReady -> Finished
            NoInput prev -> case puzzleIn of
                Nothing -> NoInput prev
                Just (Height inp) -> InputReady inp prev
                Just EOF -> ResultReady
            InputReady inp prev -> case puzzleIn of
                Nothing -> NoInput inp
                Just (Height next) -> InputReady next inp
                Just EOF -> ResultReady
            
        count' = case phase of
            Finished -> count
            NoInput val -> count
            InputReady input prev -> if input > prev
                then count + 1
                else count

        output = case phase of
            ResultReady -> Just count
            _ -> Nothing


        state' = (count', phase')

type SerializerState = (Maybe (Unsigned 32), Index 9)
emptySerializerState = (Nothing, 0)

serializerMealy :: SerializerState -> Maybe PuzzleOutput -> (SerializerState, Maybe SerialByte)
serializerMealy (number, digitIdx) result = ((number', digitIdx'), fromIntegral <$> byte)
    where
        number' = case number of
            Nothing -> result
            Just n -> Just $ n <<< 4
        
        digitIdx' = case number of
            Just _ -> satAdd SatBound digitIdx 1
            Nothing -> 0
        
        byte = if digitIdx < maxBound
            then (\a -> toHex (a >>> 28)) <$> number
            else Nothing

        toHex a = if a <= 9
            then 0b0011_0000 .|. a
            else a + 87

-- serializerMealy :: SerializerState -> Maybe PuzzleOutput -> (SerializerState, Maybe SerialByte)
-- serializerMealy (number, digitIdx) result = (state', fromIntegral <$> byte)
--     where
--         dividers = 1000:>100:>10:>1:>Nil :: Vec 4 (Unsigned 10)
--         divider = resize $ dividers !! digitIdx 
--         digit a = (a `div` divider)
--         next a = a - (digit a) * divider
        
--         number' = case number of
--             Just s -> Just $ next s
--             Nothing -> case result of
--                 Just r -> Just r
--                 Nothing -> Nothing

--         digitIdx' = case number of
--             Just _ -> satAdd SatBound digitIdx 1
--             Nothing -> 0

--         byte = if digitIdx < maxBound
--             then (\a -> 0b0011_0000 .|. digit a) <$> number
--             else Nothing

--         state' = (number', digitIdx')