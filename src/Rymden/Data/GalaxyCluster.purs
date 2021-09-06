module Rymden.Data.GalaxyCluster where

import Prelude

import Data.Array ((!!), (..))
import Data.Array as Array
import Data.Foldable (sum)
import Data.Int (even, odd)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromJust, fromMaybe')
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Random (randomInt)
import Rymden.Data.Galaxy (Galaxy, GalaxyArm)
import Rymden.Data.GalaxyCenter (cellPositionToCenterPosition, getGalaxyCenterPosition, reflectCell)
import Rymden.Data.Position (Position, down, hasFourNeighbours, hasSquare, hasTwoNeighbours, left, leftUpRightDown, right, up)
import Rymden.Helper.Foldable ((<$$>))
import Rymden.Helper.Random (chooseOne, randomBoolean)
import Data.Int (toNumber)

type GalaxyCluster
  =
  { width :: Int
  , height :: Int
  , galaxies :: Set Galaxy
  }

generateGalaxies :: Int -> Int -> Effect (Set Galaxy)
generateGalaxies width height = expandGalaxies initialAvailablePositions Set.empty
  where
  initialAvailablePositions :: Set Position
  initialAvailablePositions =
    Set.fromFoldable do
      row <- 0 .. (height - 1)
      column <- 0 .. (width - 1)
      pure $ Tuple row column

  expandGalaxies :: Set Position -> Set Galaxy -> Effect (Set Galaxy)
  expandGalaxies availablePositions galaxies =
    case Set.findMin availablePositions of
      Nothing -> pure galaxies
      Just availablePosition -> do
        maybeExistingGalaxy <- chooseOne galaxies
        case maybeExistingGalaxy of
          Nothing -> startNewGalaxy availablePositions galaxies availablePosition
          Just galaxy -> do
            startNewGalaxyAnyway <- randomBoolean (1.0 / (toNumber (Set.size galaxies) + 1.0))
            if startNewGalaxyAnyway then
              startNewGalaxy availablePositions galaxies availablePosition
            else do
              maybeExpandedGalaxy <- expandGalaxy availablePositions galaxy
              case maybeExpandedGalaxy of
                Nothing -> do
                  expandedGalaxies <- expandGalaxies availablePositions (Set.delete galaxy galaxies)
                  pure $ Set.insert galaxy expandedGalaxies
                Just expandedGalaxy ->
                  expandGalaxies
                    (Set.difference availablePositions expandedGalaxy)
                    (Set.insert expandedGalaxy $ Set.delete galaxy galaxies)

  startNewGalaxy :: Set Position -> Set Galaxy -> Position -> Effect (Set Galaxy)
  startNewGalaxy availablePositions galaxies availablePosition = do
    galaxy <- fromMaybe' (\_ -> Set.singleton availablePosition) <$> newGalaxy availablePositions
    expandGalaxies (Set.difference availablePositions galaxy) (Set.insert galaxy galaxies)

  newGalaxy :: Set Position -> Effect (Maybe Galaxy)
  newGalaxy availablePositions = do
    maybeFive <- newFive
    case maybeFive of
      Just five -> pure $ Just five
      Nothing -> do
        maybeFour <- newFour
        case maybeFour of
          Just four -> pure $ Just four
          Nothing -> do
            maybeThree <- newThree
            case maybeThree of
              Just three -> pure $ Just three
              Nothing -> do
                maybeTwo <- newTwo
                case maybeTwo of
                  Just two -> pure $ Just two
                  Nothing -> newOne
    where
    newFive :: Effect (Maybe Galaxy)
    newFive = pure Nothing

    newFour :: Effect (Maybe Galaxy)
    newFour = pure Nothing

    newThree :: Effect (Maybe Galaxy)
    newThree = pure Nothing

    newTwo :: Effect (Maybe Galaxy)
    newTwo = pure Nothing

    newOne :: Effect (Maybe Galaxy)
    newOne = Set.singleton <$$> chooseOne availablePositions

  expandGalaxy :: Set Position -> Galaxy -> Effect (Maybe Galaxy)
  expandGalaxy availablePositions galaxy = do
    maybeNeighbour <- chooseOne candidatePositions
    pure case maybeNeighbour of
      Just neighbour -> Just $ Set.insert (reflectCell galaxyCenterPosition neighbour) $ Set.insert neighbour galaxy
      Nothing -> Nothing
    where
    galaxyCenterPosition :: Position
    galaxyCenterPosition = getGalaxyCenterPosition galaxy

    candidatePositions :: Set Position
    candidatePositions =
      let
        availableNeighbours :: Set Position
        availableNeighbours =
          galaxy
            # Array.fromFoldable
            # Array.concatMap leftUpRightDown
            # Array.nub
            # Array.filter (flip Set.member availablePositions)
            # Set.fromFoldable

        reflectionIsAvailable :: Position -> Boolean
        reflectionIsAvailable position =
          Set.member (reflectCell galaxyCenterPosition position) availablePositions
      in
        Set.filter reflectionIsAvailable availableNeighbours

generateCluster :: Int -> Int -> Effect GalaxyCluster
generateCluster width height = do
  galaxies <- generateGalaxies
  pure { width, height, galaxies }
  where
  generateGalaxies :: Effect (Set Galaxy)
  generateGalaxies = do
    galaxies <-
      generateGalaxies'
        $ Set.fromFoldable do
          row <- 0 .. (height - 1)
          column <- 0 .. (width - 1)
          pure $ Tuple row column
    pure $ Set.fromFoldable galaxies
    where
    generateGalaxies' :: Set Position -> Effect (List Galaxy)
    generateGalaxies' availablePositions = do
      maybeGalaxy <- generateGalaxy availablePositions
      case maybeGalaxy of
        Just galaxy -> do
          galaxies <- generateGalaxies' $ Set.difference availablePositions galaxy
          pure $ galaxy : galaxies
        Nothing -> pure Nil

  chooseCenter :: Set Position -> Effect (Maybe Position)
  chooseCenter availablePositions = do
    maybeFourNeighbours <- chooseOne $ Set.filter (hasFourNeighbours availablePositions) availablePositions
    case maybeFourNeighbours of
      Just fourNeighbours -> pure $ Just $ cellPositionToCenterPosition fourNeighbours
      Nothing -> do
        maybeSquare <- chooseOne $ Set.filter (hasSquare availablePositions) availablePositions
        case maybeSquare of
          Just (Tuple row column) -> pure $ Just $ Tuple (row * 2 + 2) (column * 2 + 2)
          Nothing -> do
            maybeTwoNeighbours <- chooseOne $ Set.filter (hasTwoNeighbours availablePositions) availablePositions
            case maybeTwoNeighbours of
              Just twoNeighbours -> pure $ Just $ cellPositionToCenterPosition twoNeighbours
              Nothing -> cellPositionToCenterPosition <$$> chooseOne availablePositions

  generateGalaxy :: Set Position -> Effect (Maybe Galaxy)
  generateGalaxy availablePositions = do
    maybeCenter <- chooseCenter availablePositions
    case maybeCenter of
      Nothing -> pure Nothing
      Just centerPosition ->
        let
          centerRow = fst centerPosition

          centerColumn = snd centerPosition
        in
          if odd centerRow && odd centerColumn then -- center in middle of cell
            let
              center = Tuple (centerRow / 2) (centerColumn / 2)
              l = left center
              u = up center
              r = right center
              d = down center
              initialGalaxy1 = Set.singleton center
              initialGalaxy2 = if Set.member l availablePositions then Set.insert l initialGalaxy1 else initialGalaxy1
              initialGalaxy3 = if Set.member u availablePositions then Set.insert u initialGalaxy2 else initialGalaxy2
              initialGalaxy4 = if Set.member r availablePositions then Set.insert r initialGalaxy3 else initialGalaxy3
              initialGalaxy = if Set.member d availablePositions then Set.insert d initialGalaxy4 else initialGalaxy4
              availablePositions' = Set.difference availablePositions initialGalaxy
            in
              do
                galaxy <- extendGalaxy availablePositions' initialGalaxy
                pure $ Just galaxy
          else if odd centerRow then -- center on vertical border
            let
              r = Tuple (centerRow / 2) (centerColumn / 2)
              l = left r
              initialGalaxy = Set.fromFoldable [ l, r ]
              availablePositions' = Set.difference availablePositions initialGalaxy
            in
              do
                galaxy <- extendGalaxy availablePositions' initialGalaxy
                pure $ Just galaxy
          else if odd centerColumn then -- center on horizontal border
            let
              d = Tuple (centerRow / 2) (centerColumn / 2)
              u = up d
              initialGalaxy = Set.fromFoldable [ u, d ]
              availablePositions' = Set.difference availablePositions initialGalaxy
            in
              do
                galaxy <- extendGalaxy availablePositions' initialGalaxy
                pure $ Just galaxy
          else -- center on corner
            let
              bottomRight = Tuple (centerRow / 2) (centerColumn / 2)
              topRight = up bottomRight
              bottomLeft = left bottomRight
              topLeft = up bottomLeft
              initialGalaxy = Set.fromFoldable
                [ bottomRight
                , topRight
                , bottomLeft
                , topLeft
                ]
              availablePositions' = Set.difference availablePositions initialGalaxy
            in
              do
                galaxy <- extendGalaxy availablePositions' initialGalaxy
                pure $ Just galaxy

extendGalaxy :: Set Position -> Galaxy -> Effect (Galaxy)
extendGalaxy availablePositions galaxy =
  let
    galaxyCenterPosition :: Position
    galaxyCenterPosition = getGalaxyCenterPosition galaxy

    availableNeighbours :: Set Position
    availableNeighbours =
      galaxy
        # Array.fromFoldable
        # Array.concatMap leftUpRightDown
        # Array.nub
        # Array.filter (flip Set.member availablePositions)
        # Set.fromFoldable

    symmetricallyAvailableNeighbours :: Set Position
    symmetricallyAvailableNeighbours =
      availableNeighbours
        # Set.filter (flip Set.member availableNeighbours <<< reflectCell galaxyCenterPosition)

    numberOfNeighboursInGalaxy :: Position -> Int
    numberOfNeighboursInGalaxy position =
      sum
        [ if Set.member (left position) galaxy then 1 else 0
        , if Set.member (up position) galaxy then 1 else 0
        , if Set.member (right position) galaxy then 1 else 0
        , if Set.member (down position) galaxy then 1 else 0
        ]

    neighboursAndConnectivity :: Array (Tuple Position Int)
    neighboursAndConnectivity =
      symmetricallyAvailableNeighbours
        # Array.fromFoldable
        <#> (\p -> Tuple p (numberOfNeighboursInGalaxy p))

    neighboursWithConnectivityN :: Int -> Array Position
    neighboursWithConnectivityN n =
      neighboursAndConnectivity
        # Array.filter (snd >>> (==) n)
        <#> fst

    chooseNeighbour :: Effect (Maybe Position)
    chooseNeighbour =
      let
        nwc1 = neighboursWithConnectivityN 1
      in
        do
          ext1 <- randomBoolean 1.0
          ext2 <- randomBoolean 0.0
          ext3 <- randomBoolean 0.0
          ext4 <- randomBoolean 0.0
          if ext1 && Array.length nwc1 > 0 then
            chooseOne nwc1
          else
            let
              nwc2 = neighboursWithConnectivityN 2
            in
              if ext2 && Array.length nwc2 > 0 then
                chooseOne nwc2
              else
                let
                  nwc3 = neighboursWithConnectivityN 3
                in
                  if ext3 && Array.length nwc3 > 0 then
                    chooseOne nwc3
                  else
                    let
                      nwc4 = neighboursWithConnectivityN 4
                    in
                      if ext4 then
                        chooseOne nwc4
                      else pure Nothing
  in
    do
      chosenNeighbour <- chooseNeighbour
      case chosenNeighbour of
        Just position ->
          let
            reflectedPosition = reflectCell galaxyCenterPosition position
            galaxy' =
              galaxy
                # Set.insert position
                # Set.insert reflectedPosition
            availablePositions' =
              availablePositions
                # Set.delete position
                # Set.delete reflectedPosition
          in
            extendGalaxy availablePositions' galaxy'
        Nothing -> pure galaxy
