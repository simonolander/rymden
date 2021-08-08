module Rymden.Data.Board where

import Prelude
import Data.Array ((..))
import Data.Array as Array
import Data.Foldable (maximum, minimum)
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Rymden.Data.BoardErrors (BoardErrors)
import Rymden.Data.BorderSegment (BorderSegment)
import Rymden.Data.Galaxy (Galaxy)
import Rymden.Data.GalaxyCenter (GalaxyCenter)
import Rymden.Data.GalaxyCluster (GalaxyCluster, generateCluster)
import Rymden.Data.Position (Position, down, left, right, up)
import Rymden.Helper.Foldable (count)
import Rymden.Helper.Foldable ((<$$>), (<$$$>))
import Rymden.Helper.Foldable (inverseMap)

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

getPositions :: Board -> Array Position
getPositions { width, height } = do
  row <- 0 .. (height - 1)
  column <- 0 .. (width - 1)
  pure $ Tuple row column

getConnectedComponents :: Board -> Set (Set Position)
getConnectedComponents board = getConnectedComponents' $ Set.fromFoldable $ getPositions board
  where
  getConnectedComponents' :: Set Position -> Set (Set Position)
  getConnectedComponents' remainingPositions = case Set.findMin remainingPositions of
    Just position ->
      let
        component :: Set Position
        component = getConnectedComponent position remainingPositions

        remainingPositions' :: Set Position
        remainingPositions' = Set.difference remainingPositions component
      in
        Set.insert component $ getConnectedComponents' remainingPositions'
    Nothing -> Set.empty

  getConnectedComponent :: Position -> Set Position -> Set Position
  getConnectedComponent position remainingPositions =
    let
      left' :: Position
      left' = left position

      down' :: Position
      down' = down position

      up' :: Position
      up' = up position

      right' :: Position
      right' = right position

      downRight' :: Position
      downRight' = down right'

      borderLeft :: BorderSegment
      borderLeft = Tuple position down'

      borderUp :: BorderSegment
      borderUp = Tuple position right'

      borderRight :: BorderSegment
      borderRight = Tuple right' downRight'

      borderDown :: BorderSegment
      borderDown = Tuple down' downRight'

      componentCenter :: Set Position
      componentCenter = Set.singleton position

      remainingPositionsCenter :: Set Position
      remainingPositionsCenter = Set.delete position remainingPositions

      componentLeft :: Set Position
      componentLeft =
        if Set.member left' remainingPositionsCenter && not Set.member borderLeft board.borderSegments then
          getConnectedComponent left' remainingPositionsCenter
        else
          componentCenter

      remainingPositionsLeft :: Set Position
      remainingPositionsLeft = Set.difference remainingPositionsCenter componentLeft

      componentUp :: Set Position
      componentUp =
        if Set.member up' remainingPositionsLeft && not Set.member borderUp board.borderSegments then
          getConnectedComponent up' remainingPositionsLeft
        else
          componentLeft

      remainingPositionsUp :: Set Position
      remainingPositionsUp = Set.difference remainingPositionsLeft componentUp

      componentRight :: Set Position
      componentRight =
        if Set.member right' remainingPositionsUp && not Set.member borderRight board.borderSegments then
          getConnectedComponent right' remainingPositionsUp
        else
          componentUp

      remainingPositionsRight :: Set Position
      remainingPositionsRight = Set.difference remainingPositionsUp componentRight

      componentDown :: Set Position
      componentDown =
        if Set.member down' remainingPositionsRight && not Set.member borderDown board.borderSegments then
          getConnectedComponent down' remainingPositionsRight
        else
          componentRight
    in
      Set.unions
        [ componentCenter
        , componentLeft
        , componentUp
        , componentRight
        , componentDown
        ]

checkSolution :: Board -> BoardErrors
checkSolution board =
  let
    danglingPositions :: Set Position
    danglingPositions =
      board.borderSegments
        # Array.fromFoldable
        <#> (\(Tuple p1 p2) -> [ p1, p2 ])
        # join
        # count
        # Map.filterWithKey (\(Tuple row col) n -> n == 1 && row > 0 && col > 0 && row < board.height && col < board.width)
        # Map.keys

    isBorderDangling :: BorderSegment -> Boolean
    isBorderDangling (Tuple p1 p2) = Set.member p1 danglingPositions || Set.member p2 danglingPositions

    danglingBorders :: Set BorderSegment
    danglingBorders = Set.filter isBorderDangling board.borderSegments

    connectedComponents :: Set (Set Position)
    connectedComponents = getConnectedComponents board

    getConnectedComponentByPosition :: Position -> Maybe (Set Position)
    getConnectedComponentByPosition position = Array.find (Set.member position) $ Array.fromFoldable connectedComponents

    componentByCenterMap :: Map Position (Set Position)
    componentByCenterMap =
      board.centers
        <#> _.position
        <#> ( \(Tuple r c) -> case getConnectedComponentByPosition (Tuple (r / 2) (c / 2)) of
              Just component -> Just (Tuple (Tuple r c) component)
              Nothing -> Nothing
          )
        # Array.catMaybes
        # Map.fromFoldable

    centersByComponentMap :: Map (Set Position) (Set Position)
    centersByComponentMap = inverseMap componentByCenterMap

    componentsWithoutCenters :: Set (Set Position)
    componentsWithoutCenters = Set.difference connectedComponents (Map.keys centersByComponentMap)

    incorrectGalaxySizes :: Set Position
    incorrectGalaxySizes = Set.fromFoldable $ _.position <$> Array.filter hasIncorrectGalaxySize board.centers
      where
      hasIncorrectGalaxySize :: GalaxyCenter -> Boolean
      hasIncorrectGalaxySize center =
        let
          position :: Position
          position = Tuple (fst center.position / 2) (snd center.position / 2)
        in
          case getConnectedComponentByPosition position of
            Just component -> Set.size component /= center.galaxySize
            Nothing -> false

    cellsInComponentsWithoutCenter :: Set Position
    cellsInComponentsWithoutCenter = Set.unions componentsWithoutCenters
  in
    { danglingBorders
    , incorrectGalaxySizes
    , cellsInComponentsWithoutCenter
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
