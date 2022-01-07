{-# LANGUAGE NumericUnderscores #-}
module Test where

import qualified Clash.Prelude as C
import Prelude

import Types
import qualified Puzzle
import Serial

import Data.Char
import Data.List
import Data.Maybe

-- simulator should probably be in verilog kinda
-- but a "lo-fi" simulator in clash is convenient...
clocksPerBaud :: Int
clocksPerBaud = 217


parser :: IO [Maybe PuzzleInput]
parser = do
    chars <- readFile "input.txt"
    let pureSerialInput = map (fromIntegral . ord) chars
    let serialInput = intertool nops (map Just pureSerialInput)
    let result = C.simulate @C.System Puzzle.parser serialInput
    pure result
    where
        nops = take clocksPerBaud $ repeat Nothing


solver :: IO [Maybe PuzzleOutput]
solver = do
    parserOut <- parser
    return $ C.simulate @C.System Puzzle.solver parserOut


serializer :: IO [Maybe SerialByte]
serializer = do
    solverOut <- solver
    return $ C.simulate @C.System Puzzle.serializer solverOut


test :: Show a => IO [Maybe a] -> IO ()
test action = do
    result <- action
    mapM_ print (catMaybes result)

testSerial = do
    solverOut <- solver
    let simOut = C.simulate @C.System Puzzle.serializer solverOut
    let mChars = map (\a -> (chr . fromIntegral) <$> a) simOut
    return $ catMaybes mChars


intertool :: [a] -> [a] -> [a]
intertool _ [] = []
intertool sep (x:xs) = x : (sep ++ (intertool sep xs))


