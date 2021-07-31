module Rymden.Component.Board where

import Prelude
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Rymden.Component.Helpers.Property (sclass)
import Rymden.Data.Route (Route(..))
import Rymden.Component.Helpers.Property (href)
import Rymden.Content.Campaigns (campaigns)
import Halogen.Store.Monad (class MonadStore)
import Rymden.Data.Store (Store)
import Rymden.Data.Progress (Progress)
import Halogen.Store.Connect (Connected)
import Halogen.Store.Connect (connect)
import Halogen.Store.Select (selectAll)
import Data.Maybe (Maybe(..))
import Halogen.Store.Select (selectEq)
import Rymden.Data.Progress (isLevelCompleted)
import Rymden.Data.Board (Board)
import Rymden.Data.Board as Board
import Halogen.Svg.Elements as SE
import Halogen.Svg.Attributes as SA
import Data.Newtype (wrap)
import Data.Int (toNumber)
import Data.Array ((..))
import Data.Array as Array
import Data.Set as Set
import Data.Tuple (Tuple(..))

type State
  = { board :: Maybe Board
    , width :: Number
    , height :: Number
    }

type Input
  = Unit

type StoreInput
  = Connected Progress Input

data Action
  = Receive StoreInput
  | Initialize

width :: Int
width = 10

height :: Int
height = 10

component ::
  forall query output storeAction m.
  MonadEffect m =>
  MonadStore storeAction Store m =>
  H.Component query Input output m
component = connect (selectEq _.progress) $ H.mkComponent { initialState, render, eval }
  where
  initialState :: StoreInput -> State
  initialState { context, input } =
    { board: Nothing
    , width: toNumber width * 80.0
    , height: toNumber height * 80.0
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
        ]
        $ Array.concat
            [ corners
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
              ]

      horizontalBorders :: Array (HH.HTML (H.ComponentSlot slots m Action) Action)
      horizontalBorders = do
        r <- 0 .. board.height
        c <- 0 .. (board.width - 1)
        pure
          $ SE.rect
              [ SA.width cellWidth
              , SA.height (borderHeight + 1.0)
              , SA.x $ horizontalBorderOffsetX c
              , SA.y $ horizontalBorderOffsetY r - 0.5
              , sclass $ classes r c
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
        c <- 0 .. board.width
        pure
          $ SE.rect
              [ SA.width (borderWidth + 1.0)
              , SA.height cellHeight
              , SA.x $ verticalBorderOffsetX c - 0.5
              , SA.y $ verticalBorderOffsetY r
              , sclass $ classes r c
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
        r <- 0 .. board.height
        c <- 0 .. board.width
        pure
          $ SE.rect
              [ SA.width (borderWidth + 1.0)
              , SA.height (borderHeight + 1.0)
              , SA.x $ cornerOffsetX c - 0.5
              , SA.y $ cornerOffsetY r - 0.5
              ]

      centers :: Array (HH.HTML (H.ComponentSlot slots m Action) Action)
      centers = do
        (Tuple r c) <- board.centers
        pure
          $ SE.circle
              [ SA.r ((borderWidth + borderHeight) / 3.0)
              , SA.cx $ centerX c
              , SA.cy $ centerY r
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
      borderWidth = state.width / (toNumber board.width + (toNumber board.width + 1.0) / borderByCellRatio)

      borderHeight :: Number
      borderHeight = state.height / (toNumber board.height + (toNumber board.height + 1.0) / borderByCellRatio)

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
      Receive input -> H.put $ initialState input
      Initialize -> do
        board <- H.liftEffect $ Board.generate width height
        H.modify_ _ { board = Just board }
