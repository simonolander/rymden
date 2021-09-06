module Rymden.Data.GalaxyCenter where

import Prelude

import Data.Array as Array
import Data.Foldable (maximum, minimum)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Tuple (Tuple(..), fst, snd)
import Rymden.Data.Position (Position)

type GalaxyCenter
  =
  { position :: Position
  , galaxySize :: Int
  }

cellPositionToCenterPosition :: Position -> Position
cellPositionToCenterPosition (Tuple row column) = Tuple (row * 2 + 1) (column * 2 + 1)

getGalaxyCenterPosition :: Set Position -> Position
getGalaxyCenterPosition galaxy =
  let
    galaxyPositions :: Array Position
    galaxyPositions = Array.fromFoldable galaxy

    minRow :: Int
    minRow = fromMaybe 0 $ minimum $ fst <$> galaxyPositions

    maxRow :: Int
    maxRow = fromMaybe 0 $ maximum $ fst <$> galaxyPositions

    minColumn :: Int
    minColumn = fromMaybe 0 $ minimum $ snd <$> galaxyPositions

    maxColumn :: Int
    maxColumn = fromMaybe 0 $ maximum $ snd <$> galaxyPositions

    centerRow :: Int
    centerRow = maxRow + minRow + 1

    centerColumn :: Int
    centerColumn = maxColumn + minColumn + 1
  in
    Tuple centerRow centerColumn

reflectCell :: Position -> Position -> Position
reflectCell (Tuple centerRow centerColumn) (Tuple row column) =
  Tuple (centerRow - row - 1) (centerColumn - column - 1)
