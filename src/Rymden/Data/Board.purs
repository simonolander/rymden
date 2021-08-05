module Rymden.Data.Board where

import Prelude
import Rymden.Data.BorderSegment (BorderSegment)
import Rymden.Data.Position (Position)
import Effect (Effect)
import Rymden.Data.GalaxyCluster (GalaxyCluster, generateCluster)
import Rymden.Data.Galaxy (Galaxy)
import Data.Array as Array
import Data.Tuple (Tuple(..), fst, snd)
import Data.Maybe (Maybe(..))
import Rymden.Data.Position (up)
import Data.Set as Set
import Data.Set (Set)
import Rymden.Data.Position (right)
import Rymden.Data.Position (left)
import Rymden.Data.Position (down)
import Data.Foldable (minimum)
import Data.Foldable (maximum)
import Data.Maybe (fromMaybe)
import Rymden.Data.GalaxyCenter (GalaxyCenter)

type Board
  = { width :: Int
    , height :: Int
    , borderSegments :: Set BorderSegment
    , centers :: Array GalaxyCenter
    }

empty :: Int -> Int -> Board
empty width height = { width, height, borderSegments: Set.empty, centers: [] }

generate :: Int -> Int -> Effect Board
generate width height = fromGalaxyCluster <$> generateCluster width height

toggleBorderSegment :: BorderSegment -> Board -> Board
toggleBorderSegment borderSegment board =
  board
    { borderSegments =
      if Set.member borderSegment board.borderSegments then
        Set.delete borderSegment board.borderSegments
      else
        Set.insert borderSegment board.borderSegments
    }

debugFromGalaxyCluster :: GalaxyCluster -> Board
debugFromGalaxyCluster cluster = (fromGalaxyCluster cluster) { borderSegments = Set.unions $ getGalaxyBorder <$> Array.fromFoldable cluster.galaxies }

fromGalaxyCluster :: GalaxyCluster -> Board
fromGalaxyCluster cluster =
  { width: cluster.width
  , height: cluster.height
  , centers: getGalaxyCenter <$> Array.fromFoldable cluster.galaxies
  , borderSegments: Set.empty
  }

getGalaxyCenter :: Galaxy -> GalaxyCenter
getGalaxyCenter galaxy =
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

    position :: Position
    position = Tuple centerRow centerColumn

    galaxySize :: Int
    galaxySize = Set.size galaxy
  in
  { position, galaxySize }

getGalaxyBorder :: Galaxy -> Set BorderSegment
getGalaxyBorder galaxy =
  Set.fromFoldable
    $ join do
        position <- Array.fromFoldable galaxy
        pure
          $ Array.catMaybes
              [ if left position `Set.member` galaxy then
                  Nothing
                else
                  Just $ Tuple position (down position)
              , if up position `Set.member` galaxy then
                  Nothing
                else
                  Just $ Tuple position (right position)
              , if right position `Set.member` galaxy then
                  Nothing
                else
                  Just $ Tuple (right position) (down $ right position)
              , if down position `Set.member` galaxy then
                  Nothing
                else
                  Just $ Tuple (down position) (down $ right position)
              ]
