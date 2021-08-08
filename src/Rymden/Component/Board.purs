module Rymden.Component.Board where

import Prelude
import Data.Array ((..))
import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import Rymden.Component.Helpers.Property (sclass)
import Rymden.Data.Board (Board, toggleBorderSegment)
import Rymden.Data.Board as Board
import Rymden.Data.BorderSegment (BorderSegment)
import Rymden.Data.Store (Store)
import Rymden.Data.WindowProperties (WindowProperties)

type State
  = { board :: Maybe Board
    , width :: Number
    , height :: Number
    }

type Input
  = Unit

type StoreInput
  = Connected WindowProperties Input

data Action
  = Receive StoreInput
  | Initialize
  | ClickedBorder BorderSegment

width :: Int
width = 10

height :: Int
height = width

component ::
  forall query output storeAction m.
  MonadEffect m =>
  MonadStore storeAction Store m =>
  H.Component query Input output m
component = connect (selectEq _.window) $ H.mkComponent { initialState, render, eval }
  where
  initialState :: StoreInput -> State
  initialState { context, input } =
    { board: Nothing
    , width: min (toNumber context.width) (toNumber context.height)
    , height: min (toNumber context.width) (toNumber context.height)
    }

  render :: forall slots. State -> HH.HTML (H.ComponentSlot slots m Action) Action
  render state = case state.board of
    Just board -> renderBoard board
    Nothing -> HH.text "loading"
    where
    renderBoard :: Board -> HH.HTML (H.ComponentSlot slots m Action) Action
    renderBoard board =
      SE.svg
        [ SA.width state.width
        , SA.height state.height
        , sclass "board"
        ]
        $ Array.concat
            [ outerBorders
            , corners
            , cells
            , horizontalBorders
            , verticalBorders
            , centers
            ]
      where
      cells :: Array (HH.HTML (H.ComponentSlot slots m Action) Action)
      cells = do
        r <- 0 .. (board.height - 1)
        c <- 0 .. (board.width - 1)
        pure
          $ SE.rect
              [ SA.width cellWidth
              , SA.height cellHeight
              , SA.x $ cellOffsetX c
              , SA.y $ cellOffsetY r
              , sclass "cell"
              ]

      horizontalBorders :: Array (HH.HTML (H.ComponentSlot slots m Action) Action)
      horizontalBorders = do
        r <- 1 .. (board.height - 1)
        c <- 0 .. (board.width - 1)
        pure
          $ SE.rect
              [ SA.width cellWidth
              , SA.height borderHeight
              , SA.x $ horizontalBorderOffsetX c
              , SA.y $ horizontalBorderOffsetY r
              , sclass $ classes r c
              , HE.onClick $ const $ ClickedBorder $ Tuple (Tuple r c) (Tuple r (c + 1))
              ]
        where
        classes :: Int -> Int -> String
        classes row column =
          if Set.member (Tuple (Tuple row column) (Tuple row (column + 1))) board.borderSegments then
            "border active"
          else
            "border"

      verticalBorders :: Array (HH.HTML (H.ComponentSlot slots m Action) Action)
      verticalBorders = do
        r <- 0 .. (board.height - 1)
        c <- 1 .. (board.width - 1)
        pure
          $ SE.rect
              [ SA.width borderWidth
              , SA.height cellHeight
              , SA.x $ verticalBorderOffsetX c
              , SA.y $ verticalBorderOffsetY r
              , sclass $ classes r c
              , HE.onClick $ const $ ClickedBorder $ Tuple (Tuple r c) (Tuple (r + 1) c)
              ]
        where
        classes :: Int -> Int -> String
        classes row column =
          if Set.member (Tuple (Tuple row column) (Tuple (row + 1) column)) board.borderSegments then
            "border active"
          else
            "border"

      corners :: Array (HH.HTML (H.ComponentSlot slots m Action) Action)
      corners = do
        r <- 1 .. (board.height - 1)
        c <- 1 .. (board.width - 1)
        let
          hasNeighbourLeft :: Boolean
          hasNeighbourLeft = Set.member (Tuple (Tuple r (c - 1)) (Tuple r c)) board.borderSegments

          hasNeighbourUp :: Boolean
          hasNeighbourUp = Set.member (Tuple (Tuple (r - 1) c) (Tuple r c)) board.borderSegments

          hasNeighbourRight :: Boolean
          hasNeighbourRight = Set.member (Tuple (Tuple r c) (Tuple r (c + 1))) board.borderSegments

          hasNeighbourDown :: Boolean
          hasNeighbourDown = Set.member (Tuple (Tuple r c) (Tuple (r + 1) c)) board.borderSegments

          offsetLeft :: Number
          offsetLeft = cornerOffsetX c - 0.5

          offsetUp :: Number
          offsetUp = cornerOffsetY r - 0.5

          offsetRight :: Number
          offsetRight = offsetLeft + borderWidth + 1.0

          offsetDown :: Number
          offsetDown = offsetUp + borderHeight + 1.0
        pure
          if hasNeighbourLeft && hasNeighbourRight || hasNeighbourUp && hasNeighbourDown then
            SE.rect
              [ SA.width $ borderWidth + 1.0
              , SA.height $ borderHeight + 1.0
              , SA.x $ cornerOffsetX c - 0.5
              , SA.y $ cornerOffsetY r - 0.5
              , sclass "corner active"
              ]
          else if hasNeighbourLeft && hasNeighbourUp then
            SE.path
              [ SA.d
                  [ SA.m SA.Abs offsetLeft offsetUp
                  , SA.l SA.Abs offsetRight offsetUp
                  , SA.l SA.Abs offsetLeft offsetDown
                  , SA.z
                  ]
              , sclass "corner active"
              ]
          else if hasNeighbourUp && hasNeighbourRight then
            SE.path
              [ SA.d
                  [ SA.m SA.Abs offsetRight offsetUp
                  , SA.l SA.Abs offsetRight offsetDown
                  , SA.l SA.Abs offsetLeft offsetUp
                  , SA.z
                  ]
              , sclass "corner active"
              ]
          else if hasNeighbourRight && hasNeighbourDown then
            SE.path
              [ SA.d
                  [ SA.m SA.Abs offsetRight offsetDown
                  , SA.l SA.Abs offsetLeft offsetDown
                  , SA.l SA.Abs offsetRight offsetUp
                  , SA.z
                  ]
              , sclass "corner active"
              ]
          else if hasNeighbourDown && hasNeighbourLeft then
            SE.path
              [ SA.d
                  [ SA.m SA.Abs offsetLeft offsetDown
                  , SA.l SA.Abs offsetLeft offsetUp
                  , SA.l SA.Abs offsetRight offsetDown
                  , SA.z
                  ]
              , sclass "corner active"
              ]
          else
            HH.text ""

      centers :: Array (HH.HTML (H.ComponentSlot slots m Action) Action)
      centers = do
        center <- board.centers
        let
          Tuple r c = center.position
        pure
          if false then
            SE.circle
              [ SA.r ((borderWidth + borderHeight) / 3.0)
              , SA.cx $ centerX c
              , SA.cy $ centerY r
              ]
          else
            SE.text
              [ SA.x $ centerX c
              , SA.y $ centerY r
              , SA.text_anchor SA.AnchorMiddle
              , SA.dominant_baseline SA.BaselineMiddle
              , sclass "galaxy-center"
              ]
              [ HH.text $ show center.galaxySize ]

      outerBorders :: Array (HH.HTML (H.ComponentSlot slots m Action) Action)
      outerBorders =
        [ SE.rect
            [ SA.width borderWidth
            , SA.height state.height
            , SA.x 0.0
            , SA.y 0.0
            , sclass "corner active"
            ]
        , SE.rect
            [ SA.width state.width
            , SA.height borderHeight
            , SA.x 0.0
            , SA.y 0.0
            , sclass "corner active"
            ]
        , SE.rect
            [ SA.width borderWidth
            , SA.height state.height
            , SA.x (state.width - borderWidth)
            , SA.y 0.0
            , sclass "corner active"
            ]
        , SE.rect
            [ SA.width state.width
            , SA.height borderHeight
            , SA.x 0.0
            , SA.y (state.height - borderHeight)
            , sclass "corner active"
            ]
        ]

      borderByCellRatio :: Number
      borderByCellRatio = 0.15

      cellWidth :: Number
      cellWidth = state.width / (toNumber board.width + (toNumber board.width + 1.0) * borderByCellRatio)

      cellOffsetX :: Int -> Number
      cellOffsetX index = borderWidth + toNumber index * (borderWidth + cellWidth)

      cellHeight :: Number
      cellHeight = state.height / (toNumber board.height + (toNumber board.height + 1.0) * borderByCellRatio)

      cellOffsetY :: Int -> Number
      cellOffsetY index = borderHeight + toNumber index * (borderHeight + cellHeight)

      borderWidth :: Number
      borderWidth = state.width / (toNumber board.width / borderByCellRatio + (toNumber board.width + 1.0))

      borderHeight :: Number
      borderHeight = state.height / (toNumber board.height / borderByCellRatio + (toNumber board.height + 1.0))

      horizontalBorderOffsetX :: Int -> Number
      horizontalBorderOffsetX index = borderWidth + toNumber index * (borderWidth + cellWidth)

      horizontalBorderOffsetY :: Int -> Number
      horizontalBorderOffsetY index = toNumber index * (borderHeight + cellHeight)

      verticalBorderOffsetX :: Int -> Number
      verticalBorderOffsetX index = toNumber index * (borderWidth + cellWidth)

      verticalBorderOffsetY :: Int -> Number
      verticalBorderOffsetY index = borderHeight + toNumber index * (borderHeight + cellHeight)

      cornerOffsetX :: Int -> Number
      cornerOffsetX index = toNumber index * (borderWidth + cellWidth)

      cornerOffsetY :: Int -> Number
      cornerOffsetY index = toNumber index * (borderHeight + cellHeight)

      centerX :: Int -> Number
      centerX column = (borderWidth + toNumber column * (borderWidth + cellWidth)) / 2.0

      centerY :: Int -> Number
      centerY row = (borderHeight + toNumber row * (borderHeight + cellHeight)) / 2.0

  eval :: forall slots. H.HalogenQ query Action StoreInput ~> H.HalogenM State Action slots output m
  eval =
    H.mkEval
      $ H.defaultEval
          { handleAction = handleAction
          , receive = Just <<< Receive
          , initialize = Just Initialize
          }
    where
    handleAction = case _ of
      Receive input -> pure unit
--        H.modify_
--          _
--            { width = min (toNumber input.context.width) (toNumber input.context.height)
--            , height = min (toNumber input.context.width) (toNumber input.context.height)
--            }
      Initialize -> do
        board <- H.liftEffect $ Board.generate width height
        H.modify_ _ { board = Just board }
      ClickedBorder borderSegment -> do
        H.modify_ \state -> case state.board of
          Just board -> state { board = Just $ toggleBorderSegment borderSegment board }
          Nothing -> state
