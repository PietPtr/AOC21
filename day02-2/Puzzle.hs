{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NumericUnderscores #-}
module Puzzle where

import Clash.Prelude

import Types
import Lib
import Data.Char
import Data.Maybe
import Debug.Trace

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

        parserM = exposeEnable (mealy justParse (Newline 1))


solver :: HiddenClockResetEnable dom =>
    Signal dom (Maybe PuzzleInput) -> Signal dom (Maybe PuzzleOutput)
solver puzzleInput = result
    where
        -- wires
        enableSolver = toEnable $ isJust <$> puzzleInput
        result = solverM enableSolver (bullshitFromJust <$> puzzleInput)

        bullshitFromJust maybe = case maybe of
            Just a -> a
            Nothing -> Forward 0 -- no effect so no harm?

        -- defs
        solverM = exposeEnable (mealy justSolve (0, 0))

type SolverState = (Unsigned 16, Unsigned 16)

justSolve :: SolverState -> PuzzleInput -> (SolverState, Maybe PuzzleOutput)
justSolve (x, y) move = ((x', y'), product)
    where
        (x', y') = case move of
            Forward n -> (satAdd SatBound x (fromIntegral n), y)
            Down n -> (x, satAdd SatBound y (fromIntegral n))
            Up n -> (x, satSub SatBound y (fromIntegral n))
            EOF -> (x, y)
        
        product = case move of
            EOF -> Just $ resize x * resize y
            _ -> Nothing

serializer :: HiddenClockResetEnable dom =>
    Signal dom (Maybe PuzzleOutput) -> Signal dom (Maybe SerialByte)
serializer = mealy serializerMealy emptySerializerState

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

data ParseState 
    = Newline (Index 2)
    | WaitForward (Index 7) 
    | WaitDown (Index 4) 
    | WaitUp (Index 2) 
    | ReadNum Direcion
    | SendEOF
    | Done
    deriving (Show, Generic, NFDataX)

justParse :: ParseState -> SerialByte -> (ParseState, Maybe PuzzleInput)
justParse state inp = (state', output )
    where
        next state ctr dir = if ctr == 0
            then ReadNum dir
            else state

        inpChar = chr $ fromIntegral inp
        
        state' = case state of
            Newline 0 -> case inpChar of
                '\n' -> Newline 1
            Newline 1 -> case inpChar of
                'f' -> WaitForward maxBound
                'd' -> WaitDown maxBound
                'u' -> WaitUp maxBound
                '\n' -> SendEOF
            WaitForward ctr -> next (WaitForward $ ctr - 1) ctr DForward
            WaitDown ctr -> next (WaitDown $ ctr - 1) ctr DDown
            WaitUp ctr -> next (WaitUp $ ctr - 1) ctr DUp
            ReadNum _ -> Newline 0
            SendEOF -> Done
            Done -> Done

        output = case state of
            ReadNum dir -> case dir of
                DForward -> Forward <$> inp'
                DDown -> Down <$> inp'
                DUp -> Up <$> inp'
            SendEOF -> (const EOF) <$> inp' -- requires _three_ newlines at the end of puzzle input...
            _ -> Nothing
            where
                inp' = if inp == 0x07
                    then Nothing
                    else Just $ fromIntegral (inp .&. 0b1111)
