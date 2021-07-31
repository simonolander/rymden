module Rymden.Data.Position where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Set as Set
import Data.Set (Set)

type Position
  = Tuple Int Int

left :: Position -> Position
left (Tuple r c) = Tuple r (c - 1)

up :: Position -> Position
up (Tuple r c) = Tuple (r - 1) c

right :: Position -> Position
right (Tuple r c) = Tuple r (c + 1)

down :: Position -> Position
down (Tuple r c) = Tuple (r + 1) c

leftUpRightDown :: Position -> Array Position
leftUpRightDown position =
  [ left position
  , up position
  , right position
  , down position
  ]

hasFourNeighbours :: Set Position -> Position -> Boolean
hasFourNeighbours availablePositions position = hasLeftRight availablePositions position && hasUpDown availablePositions position

hasTwoNeighbours :: Set Position -> Position -> Boolean
hasTwoNeighbours availablePositions position = hasLeftRight availablePositions position || hasUpDown availablePositions position

hasLeftRight :: Set Position -> Position -> Boolean
hasLeftRight availablePositions position = Set.member (left position) availablePositions && Set.member (right position) availablePositions

hasUpDown :: Set Position -> Position -> Boolean
hasUpDown availablePositions position = Set.member (up position) availablePositions && Set.member (down position) availablePositions
