{-# LANGUAGE  RecordWildCards #-}
module Types where

import Clash.Prelude

import Data.Maybe

type SerialByte = BitVector 8


data Move = Forward (Index 10) | Up (Index 10) | Down (Index 10)
    deriving (Show, Generic, NFDataX)

type PuzzleInput  = Move
type PuzzleOutput = Unsigned 32
