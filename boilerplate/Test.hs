{-# LANGUAGE NumericUnderscores #-}
module Test where

import Clash.Prelude

import Types
import Puzzle
import Serial

-- simulator should probably be in verilog kinda
-- but a "lo-fi" simulator in clash is convenient...
clocksPerBaud = 217

testInput :: [Char]
testInput = "199\n200\n208\n210\n200\n207\n240\n269\n260\n263\n"

sim = simulate @System top
    where
        canSend = []