module Rymden.Component.Board where

import Prelude

import Data.Array ((..))
import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import Rymden.Component.Helpers.Property (sclass)
import Rymden.Data.Board (Board, checkSolution, clear, toggleBorderSegment)
import Rymden.Data.Board as Board
import Rymden.Data.BoardErrors (BoardErrors)
import Rymden.Data.BoardErrors as BoardErrors
import Rymden.Data.BorderSegment (BorderSegment)
import Rymden.Data.Position (Position)
import Rymden.Data.Store (Store)

type State
  =
  { board :: Maybe Board
  , width :: Number
  , height :: Number
  , highlightErrors :: Boolean
  }

type Input = Unit

data Action
  = Initialize
  | ClickedBorder BorderSegment

data Query a
  = HighlightErrors Boolean a
  | Undo a
  | Redo a
  | New a
  | Clear a

data Output
  = Solved Boolean

numberOfColumns :: Int
numberOfColumns = 2

numberOfRows :: Int
numberOfRows = numberOfColumns

component :: forall m. MonadEffect m => H.Component Query Input Output m
component = H.mkComponent { initialState, render, eval }
  where
  initialState :: Input -> State
  initialState _ =
    { board: Nothing
    , width: 1024.0
    , height: 1024.0
    , highlightErrors: false
    }

  render :: forall slots. State -> HH.HTML (H.ComponentSlot slots m Action) Action
  render state = case state.board of
    Just board -> renderBoard board
    Nothing -> HH.div_ [ HH.text "Loading..." ]
    where
    renderBoard :: Board -> HH.HTML (H.ComponentSlot slots m Action) Action
    renderBoard board =
      SE.svg
        [ SA.viewBox 0.0 0.0 state.width state.height
        , sclass "board"
        ]
        $ Array.concat
          [ outerBorders
          , corners
          , cells
          , horizontalBorders
          , verticalBorders
          , asymetricCenterIndicators
          , centers
          ]
      where
      boardErrors :: BoardErrors
      boardErrors =
        if state.highlightErrors then
          checkSolution board
        else
          BoardErrors.empty

      cells :: Array (HH.HTML (H.ComponentSlot slots m Action) Action)
      cells = do
        row <- 0 .. (board.height - 1)
        column <- 0 .. (board.width - 1)
        pure $ renderCell row column
        where
        renderCell row column =
          SE.rect
            [ SA.width cellWidth
            , SA.height cellHeight
            , SA.x $ cellOffsetX column
            , SA.y $ cellOffsetY row
            , sclass classes
            ]
          where
          position :: Position
          position = Tuple row column

          classes :: String
          classes =
            let
              missingCenterClass :: Maybe String
              missingCenterClass =
                if Set.member position boardErrors.cellsInComponentsWithoutCenter then
                  Just "missing-center"
                else
                  Nothing
            in
              String.joinWith " "
                $ Array.catMaybes
                  [ Just "cell"
                  , missingCenterClass
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
          let
            borderSegment :: BorderSegment
            borderSegment = Tuple (Tuple row column) (Tuple row (column + 1))
          in
            if Set.member borderSegment board.borderSegments then
              if Set.member borderSegment boardErrors.danglingBorders then
                "border active dangling"
              else
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
          let
            borderSegment :: BorderSegment
            borderSegment = Tuple (Tuple row column) (Tuple (row + 1) column)
          in
            if Set.member borderSegment board.borderSegments then
              if Set.member borderSegment boardErrors.danglingBorders then
                "border active dangling"
              else
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
      centers = renderGalaxyCenter <$> board.centers
        where
        renderGalaxyCenter center =
          SE.text
            [ SA.x $ centerX column
            , SA.y $ centerY row
            , SA.text_anchor SA.AnchorMiddle
            , SA.dominant_baseline SA.Central
            , sclass classes
            ]
            [ HH.text $ show center.galaxySize ]
          where
          row :: Int
          row = fst center.position

          column :: Int
          column = snd center.position

          classes :: String
          classes =
            let
              incorrectSizeClass :: Maybe String
              incorrectSizeClass =
                if Set.member center.position boardErrors.incorrectGalaxySizes then
                  Just "incorrect-size"
                else
                  Nothing
            in
              String.joinWith " "
                $ Array.catMaybes
                  [ Just "galaxy-center"
                  , incorrectSizeClass
                  ]

      asymetricCenterIndicators :: Array (HH.HTML (H.ComponentSlot slots m Action) Action)
      asymetricCenterIndicators = renderAssymetricCenterIndicator <$> Array.fromFoldable boardErrors.asymmetricCenters
        where
        renderAssymetricCenterIndicator center =
          SE.circle
            [ SA.cx $ centerX $ snd center
            , SA.cy $ centerY $ fst center
            , SA.r $ (cellWidth + cellHeight) / 8.0
            , sclass "asymmetric-center"
            ]

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

  eval :: forall slots. H.HalogenQ Query Action Input ~> H.HalogenM State Action slots Output m
  eval =
    H.mkEval
      $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , initialize = Just Initialize
        }
    where
    initialize = do
      board <- H.liftEffect $ Board.generate numberOfColumns numberOfRows
      H.modify_ _ { board = Just board }
      when (not BoardErrors.hasErrors $ checkSolution board) do
        H.raise $ Solved true

    updateBoard board = do
      let
        boardErrors = checkSolution board
      H.modify_
        _
          { board = Just board
          , highlightErrors = false
          }
      H.raise $ Solved $ not BoardErrors.hasErrors boardErrors

    handleAction = case _ of
      Initialize -> initialize
      ClickedBorder borderSegment -> do
        maybeBoard <- H.gets _.board
        case maybeBoard of
          Just board -> updateBoard $ toggleBorderSegment borderSegment board
          Nothing -> pure unit

    handleQuery :: forall a. Query a -> H.HalogenM State Action slots Output m (Maybe a)
    handleQuery = case _ of
      HighlightErrors highlightErrors a -> do
        H.modify_ _ { highlightErrors = highlightErrors }
        pure $ Just a
      Undo a -> do
        pure $ Just a
      Redo a -> do
        pure $ Just a
      New a -> do
        initialize
        pure $ Just a
      Clear a -> do
        maybeBoard <- H.gets _.board
        case maybeBoard of
          Just board -> updateBoard $ clear board
          Nothing -> pure unit
        pure $ Just a
