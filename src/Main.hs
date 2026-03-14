{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Brainfuck
import Control.Lens
import Data.Maybe
import Data.Text (Text, empty)
import Data.Text.Internal.Fusion.Size (isEmpty)
import GHC.Conc.IO (threadDelay)
import Logic
import Monomer
import qualified Monomer.Lens as L
import TextShow

data AppModel = AppModel
  { _rows :: Int,
    _columns :: Int,
    _speed :: Int,
    _gens :: Int,
    _playing :: Bool,
    _dragging :: Bool,
    _grid :: Grid,
    _focusing :: (Int, Int)
  }
  deriving (Eq, Show)

data AppEvent
  = AppInit
  | TogglePlaying
  | StartPlaying
  | StopPlaying
  | UpdateGrid
  | ReplaceGrid Grid
  | ResetGrid
  | TimerEvent
  | ResetFocus
  | FocusOn Int Int
  | RowsChanged Int
  | ColumnsChanged Int
  | FlipCell Int Int
  | DragFlip
  | Painting Int Int
  | ImportBF
  | ExportBF
  -- | Debug Text
  | EmptyEvent
  deriving (Eq, Show)

makeLenses 'AppModel

buildUI ::
  WidgetEnv AppModel AppEvent ->
  AppModel ->
  WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree
  where
    -- whether we need to render the grid this frame or not
    -- i hope this has made like any impact on performance
    mergeGrid wenv old new = old ^. grid /= new ^. grid

    -- some constants
    -- probably not the best place to put them but
    rowsMax = 50
    columnsMax = 50
    speedMax = 5000

    -- ! start of widget tree
    widgetTree =
      keystroke [("Space", DragFlip)]
        $ box_
          [ onClick ResetFocus
          -- onBtnPressed $ checkButton "empty" 0 0 True
          -- onBtnReleased $ checkButton "empty" 0 0 False
          ]
        $ vstack_
          [childSpacing_ 10]
          [ hstack_ -- ! top row
              [childSpacing_ 10]
              [ vstack_ -- ! top left sliders
                  [childSpacing_ 10]
                  [ hstack_
                      [childSpacing_ 10]
                      [ label "rows",
                        hslider_ rows 1 rowsMax [thumbVisible, onChange RowsChanged],
                        label $ showt (model ^. rows)
                      ],
                    hstack_
                      [childSpacing_ 10]
                      [ label "columns",
                        hslider_ columns 1 columnsMax [thumbVisible, onChange ColumnsChanged],
                        label $ showt (model ^. columns)
                      ],
                    hstack_
                      [childSpacing_ 10]
                      [ label "milliseconds",
                        hslider_ speed 1 speedMax [thumbVisible],
                        label $ showt (model ^. speed)
                      ]
                  ],
                -- ! top right buttons
                widgetIf (model ^. playing) $ button "Pause" TogglePlaying `styleBasic` [width 100],
                widgetIf (not $ model ^. playing) $ button "Play" TogglePlaying `styleBasic` [width 100],
                button "Reset" ResetGrid `styleBasic` [width 100],
                button_ "Step" UpdateGrid [onClick StopPlaying] `styleBasic` [width 100]
              ],
            scroll $ -- ! grid
              box_
                [mergeRequired mergeGrid]
                (vstack_ [childSpacing_ 1] [row y | y <- [0 .. model ^. rows - 1]])
                `nodeKey` "grid",
            hstack -- ! bottom row
              [ hstack -- ! bottom left info
                  [ label "gens: ",
                    label $ showt $ model ^. gens,
                    label " | ",
                    label "painting: ",
                    widgetIf (model ^. dragging) $ label "on ",
                    widgetIf (not $ model ^. dragging) $ label "off ",
                    label "(press space)"
                  ],
                filler,
                hstack_ -- ! bottom right buttons
                  [childSpacing_ 10]
                  [ button "Import BrainFuck" ImportBF,
                    button "Export BrainFuck" ExportBF
                  ]
              ]
          ]
          `styleBasic` [padding 10]
          `nodeKey` "empty"

    -- toFlipOrNotToFlip x y
    --   | not $ model ^. dragging = FlipCell x y
    --   | otherwise = ResetFocus

    -- makes a single cell
    -- consists of box for events and empty label just to hold space
    cell x y =
      -- couldn't get this to target for some reason????
      -- keystroke [("Space", toFlipOrNotToFlip x y)] $
      box_
        [ -- onClick $ FlipCell x y,
          -- onBtnPressed $ checkButton "" x y True,
          -- onBtnPressed (\_ _ -> FocusOn $ showt x <> " " <> showt y),
          onBtnPressed (\_ _ -> FlipCell x y),
          -- onBtnReleased $ checkButton "" x y False,
          onEnter $ FocusOn x y,
          onEnter $ Painting x y,
          onLeave $ FocusOn (-1) (-1)
        ]
        ( label ""
            `styleBasic` [ bgColor $ Color 255 255 255 1,
                           styleIf (getCell (model ^. grid) x y == 1) $ bgColor $ Color 0 0 0 1,
                           width 30,
                           height 30,
                           cursorHand
                         ]
        )
    -- i keep thinking i need this and then i don't lol
    -- `nodeKey` (showt x <> " " <> showt y)

    -- instead of a border i'm just using child spacing
    -- it worked out better for the focusing stuff anyways
    row y = hstack_ [childSpacing_ 1] [cell x y | x <- [0 .. model ^. columns - 1]]

handleEvent ::
  WidgetEnv AppModel AppEvent ->
  WidgetNode AppModel AppEvent ->
  AppModel ->
  AppEvent ->
  [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> [Producer $ gridUpdateProducer 1, Event ResetFocus]
  -- for changing the play state
  TogglePlaying -> [Model $ model & playing %~ not]
  StopPlaying -> [Model $ model & playing .~ False]
  StartPlaying -> [Model $ model & playing .~ True]
  -- grid manipulation events
  UpdateGrid -> [Model $ model & grid %~ nextGen & gens +~ 1, Request RenderOnce]
  ReplaceGrid g ->
    [ Model $ model & grid .~ g & rows .~ length g & columns .~ length (head g),
      -- put them all in one line because you can't have multiple of the same
      -- thing i one eent apparently
      -- Model $ model & rows .~ length g,
      -- Model $ model & columns .~ length (head g),
      Request RenderOnce
    ]
  ResetGrid -> [Model $ model & grid .~ newGrid (model ^. rows) (model ^. columns) & gens .~ 0, Event StopPlaying]
  -- for the sliders to resize the grid
  RowsChanged _ -> [resizeFunc, Event StopPlaying]
  ColumnsChanged _ -> [resizeFunc, Event StopPlaying]
  -- focus shenanigans
  ResetFocus -> [SetFocusOnKey $ WidgetKey "empty"]
  -- this is for painting mode (paint a cell when clicking space)
  FocusOn x y -> [Model $ model & focusing .~ (x, y)]
  -- timer function for the interval
  TimerEvent
    | model ^. playing -> [Event UpdateGrid, Producer $ gridUpdateProducer $ model ^. speed]
    | otherwise -> [Producer $ gridUpdateProducer $ model ^. speed]
  -- cell manip, mostly for painting and clicking on cells
  FlipCell x y ->
    [ Model $ model & grid %~ flipCell x y,
      Event StopPlaying
      -- SetFocusOnKey $ WidgetKey (showt x <> showt y)
    ]
  Painting x y | model ^. dragging -> [Event $ FlipCell x y]
  -- dragging
  DragFlip -> [Model $ model & dragging %~ not, Event $ flipFocus (model ^. focusing)]
  -- brainfuck producers
  ImportBF -> [Producer importBF, Model $ model & gens .~ 0]
  ExportBF -> [Producer $ exportBF $ model ^. grid]
  -- Debug t -> [Producer $ debug t]
  -- in case i forgot any cases or just wanted them to do nothing
  _ -> []
  where
    -- checking bounds of the (model ^. focusing) field, (-1, -1) if no cell is focused
    flipFocus (x, y)
      | (x < 0) || (y < 0) = EmptyEvent
      | otherwise = Painting x y
    resizeFunc = Model $ model & grid %~ resizeGrid (model ^. rows) (model ^. columns)

gridUpdateProducer :: Int -> (AppEvent -> IO ()) -> IO ()
gridUpdateProducer m sendMsg = do
  threadDelay $ 1000 * m
  -- sendMsg Debug
  sendMsg TimerEvent

-- didn't need this in the end i'm crine
-- checkButton :: Text -> Int -> Int -> Bool -> Button -> Int -> AppEvent
-- checkButton k x y p b i
--   | b == BtnLeft = case k of
--       "empty"
--         | p -> DragDown
--         | otherwise -> DragUp
--       _ -> FlipCell x y
--   | otherwise = Debug

importBF :: (AppEvent -> IO ()) -> IO ()
importBF sendMsg = do
  file <- readFile "input.bf"
  let parsed = fromBF file
  -- sendMsg $ Debug $ showt parsed
  sendMsg $ ReplaceGrid parsed

exportBF :: Grid -> (AppEvent -> IO ()) -> IO ()
exportBF g sendMsg =
  let text = toBF g
   in writeFile "output.bf" text

-- debug :: Text -> (AppEvent -> IO ()) -> IO ()
-- debug t sendMsg = do
--   print t

main :: IO ()
main = do
  startApp model handleEvent buildUI config
  where
    -- default grid
    usingGrid = newGrid 10 10
    gridRows = length usingGrid
    gridColumns = length $ head usingGrid

    config =
      [ appWindowTitle "conway's brain of fuck",
        appWindowIcon "./assets/images/icon.png",
        appTheme darkTheme,
        appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
        appInitEvent AppInit
      ]
    model =
      AppModel
        { _rows = gridRows,
          _columns = gridColumns,
          _speed = 500,
          _gens = 0,
          _playing = False,
          _dragging = False,
          _grid = usingGrid,
          _focusing = (-1, -1)
        }
