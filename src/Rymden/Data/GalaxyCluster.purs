module Rymden.Data.GalaxyCluster where

import Prelude
import Data.Array ((!!), (..))
import Data.Array as Array
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Debug (traceM)
import Effect (Effect)
import Effect.Random (randomInt)
import Rymden.Data.Galaxy (Galaxy, GalaxyArm)
import Rymden.Data.Position (Position, down, hasFourNeighbours, hasTwoNeighbours, left, leftUpRightDown, right, up)
import Rymden.Helper.Random (chooseOne, randomBoolean)

type GalaxyCluster
  = { width :: Int
    , height :: Int
    , galaxies :: Set Galaxy
    }

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
    traceM (Array.fromFoldable $ Array.fromFoldable <$> galaxies)
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
    fourNeighbours <- chooseOne $ Set.filter (hasFourNeighbours availablePositions) availablePositions
    case fourNeighbours of
      Just _ -> pure fourNeighbours
      Nothing -> do
        twoNeighbours <- chooseOne $ Set.filter (hasTwoNeighbours availablePositions) availablePositions
        case twoNeighbours of
          Just _ -> pure twoNeighbours
          Nothing -> chooseOne availablePositions

  generateGalaxy :: Set Position -> Effect (Maybe Galaxy)
  generateGalaxy availablePositions = do
    maybeCenter <- chooseCenter availablePositions
    case maybeCenter of
      Nothing -> pure Nothing
      Just center -> do
        let
          availablePositions' = Set.delete center availablePositions

          l = left center

          u = up center

          r = right center

          d = down center
        if Set.member l availablePositions' && Set.member u availablePositions' && Set.member r availablePositions' && Set.member d availablePositions' then do
          availablePositions'' <- extendGalaxyArms availablePositions' [ { p1: l, p2: r }, { p1: u, p2: d } ]
          Set.difference availablePositions' availablePositions''
            # Set.insert center
            # Set.insert l
            # Set.insert r
            # Set.insert u
            # Set.insert d
            # Just
            # pure
        else if Set.member l availablePositions' && Set.member r availablePositions' then do
          availablePositions'' <- extendGalaxyArms availablePositions' [ { p1: l, p2: r } ]
          Set.difference availablePositions' availablePositions''
            # Set.insert center
            # Set.insert l
            # Set.insert r
            # Just
            # pure
        else if Set.member u availablePositions' && Set.member d availablePositions' then do
          availablePositions'' <- extendGalaxyArms availablePositions' [ { p1: u, p2: d } ]
          Set.difference availablePositions' availablePositions''
            # Set.insert center
            # Set.insert u
            # Set.insert d
            # Just
            # pure
        else
          pure $ Just $ Set.singleton center

extendGalaxyArms :: Set Position -> Array GalaxyArm -> Effect (Set Position)
extendGalaxyArms availablePositions arms = do
  armIndex <- randomInt 0 (Array.length arms - 1)
  case arms !! armIndex of
    Nothing -> pure availablePositions
    Just arm -> do
      let
        availableArms' = availableArms availablePositions arm
      armIndex <- randomInt 0 (Array.length availableArms' - 1)
      case availableArms' !! armIndex of
        Nothing -> pure availablePositions
        Just nextArm -> do
          let
            availablePositions' =
              availablePositions
                # Set.delete nextArm.p1
                # Set.delete nextArm.p2
          continue <- randomBoolean 0.9
          if continue then
            extendGalaxyArms availablePositions' [ nextArm ]
          else
            pure availablePositions'

availableArms :: Set Position -> GalaxyArm -> Array GalaxyArm
availableArms availablePositions arm =
  let
    l1 :: Position
    l1 = left arm.p1

    u1 :: Position
    u1 = up arm.p1

    r1 :: Position
    r1 = right arm.p1

    d1 :: Position
    d1 = down arm.p1

    l2 :: Position
    l2 = left arm.p2

    u2 :: Position
    u2 = up arm.p2

    r2 :: Position
    r2 = right arm.p2

    d2 :: Position
    d2 = down arm.p2

    maybeLeft :: Maybe GalaxyArm
    maybeLeft =
      if Set.member l1 availablePositions && Set.member r2 availablePositions then
        Just { p1: l1, p2: r2 }
      else
        Nothing

    maybeUp :: Maybe GalaxyArm
    maybeUp =
      if Set.member u1 availablePositions && Set.member d2 availablePositions then
        Just { p1: u1, p2: d2 }
      else
        Nothing

    maybeRight :: Maybe GalaxyArm
    maybeRight =
      if Set.member r1 availablePositions && Set.member l2 availablePositions then
        Just { p1: r1, p2: l2 }
      else
        Nothing

    maybeDown :: Maybe GalaxyArm
    maybeDown =
      if Set.member d1 availablePositions && Set.member u2 availablePositions then
        Just { p1: d1, p2: u2 }
      else
        Nothing
  in
    Array.catMaybes
      [ maybeLeft
      , maybeUp
      , maybeRight
      , maybeDown
      ]

availableNeighbours :: Set Position -> Position -> Set Position
availableNeighbours availablePositions position =
  leftUpRightDown position
    # Array.filter (flip Set.member availablePositions)
    # Set.fromFoldable
