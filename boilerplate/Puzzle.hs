module Puzzle where

import Clash.Prelude

import Types
import Lib
import Data.Char
import Data.Maybe

parser :: HiddenClockResetEnable dom =>
    Signal dom (Maybe SerialByte) -> Signal dom (Maybe PuzzleInput)
parser = mealy parserMealy emptyParserState

solver :: HiddenClockResetEnable dom =>
    Signal dom (Maybe PuzzleInput) -> Signal dom (Maybe PuzzleOutput)
solver inp = pure Nothing

serializer :: HiddenClockResetEnable dom =>
    Signal dom (Maybe PuzzleOutput) -> Signal dom (Maybe SerialByte)
serializer outp = (fmap . fmap) fromIntegral outp

type ParserState = ()
emptyParserState = ()

parserMealy :: ParserState -> (Maybe SerialByte) -> (ParserState, Maybe PuzzleInput)
parserMealy state input = (state, Nothing)

