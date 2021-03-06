{-# LANGUAGE  RecordWildCards #-}
module Types where

import Clash.Prelude

import Data.Maybe

type SerialByte = BitVector 8

type PuzzleInput  = Unsigned 32
type PuzzleOutput = Unsigned 32
