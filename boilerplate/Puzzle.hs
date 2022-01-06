module Puzzle where

import Clash.Prelude

import Types

parser :: HiddenClockResetEnable dom =>
    Signal dom (Maybe SerialByte) -> Signal dom (Maybe PuzzleInput)
parser = mealy parserMealy emptyParserState

solver :: HiddenClockResetEnable dom =>
    Signal dom (Maybe PuzzleInput) -> Signal dom (Maybe PuzzleOutput)
solver inp = outp
    where
        (sending', outp) = unbundle $ f <$> inp <*> sending
        sending = register False (sending')

        f inp sending = if sending
            then (True, Just 66)
            else case inp of
            Just _ -> (True, Just 65)
            _ -> (False, Nothing)

serializer :: HiddenClockResetEnable dom =>
    Signal dom (Maybe PuzzleOutput) -> Signal dom (Maybe SerialByte)
serializer outp = (fmap . fmap) fromIntegral outp

type ParserState = ()
emptyParserState = ()

parserMealy :: ParserState -> (Maybe SerialByte) -> (ParserState, Maybe PuzzleInput)
parserMealy state input = (state, output)
    where
        output = fromIntegral <$> input

