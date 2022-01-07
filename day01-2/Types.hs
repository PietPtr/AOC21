{-# LANGUAGE  RecordWildCards #-}
module Types where

import Clash.Prelude

import Data.Maybe

type SerialByte = BitVector 8

data PuzzleInput  = Height (Unsigned 32) | EOF
    deriving (Show, Generic, NFDataX)
type PuzzleOutput = Unsigned 32
