module Logic where

-- not needed for main project functionality, here for debugging purposes
main :: IO ()
main = do
  putStrLn $ showGrid gameGrid
  putStrLn $ showGrid (nextGen gameGrid)
  putStrLn $ showGrid (setRow 0 (replicate gameWidth 1) gameGrid)

  let newCol = replicate 5 0 ++ replicate 5 1
  putStrLn $ showGrid (appendCol newCol gameGrid)

  putStrLn $ showGrid (appendRow (replicate (gameWidth+1) 1) (appendCol newCol gameGrid))

  putStrLn $ showGrid (setCell gameGrid 19 7 1)

  putStrLn $ showGrid (appendEmptyRows 20 gameGrid)
  putStrLn $ showGrid (appendEmptyCols 10 gameGrid)
  
  putStrLn $ showGrid (resizeGrid 3 25 gameGrid)

  let bfStrop = [0, 1, 1, 1, 0, 0, 0, 1, 0, 1]
  putStrLn $ showGrid (make2D bfStrop)

-- GAME LOGIC

type Grid = [[Int]] -- 0 for dead, 1 for alive

gameHeight :: Int
gameHeight = 10

gameWidth :: Int
gameWidth = 20

-- starting grid for now
-- just to test out logic is correct
gameGrid :: Grid
gameGrid =
  [ replicate gameWidth 0,
    [0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0],
    [0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0]
  ]
    ++ replicate (gameHeight - 4) (replicate gameWidth 0)

newGrid :: Int -> Int -> Grid
newGrid rows cols = [[0 | _ <- [0 .. cols - 1]] | _ <- [0 .. rows - 1]]

-- get value at specified grid cell
-- -1 for not valid grid cell (is this safe?)
-- x is horizontal axis, y is vertical (like standard cartesian grid)
getCell :: Grid -> Int -> Int -> Int
getCell grid x y
  -- out of bounds = 0 makes neighors easier
  -- will assume is always valid grid cell call
  | x < 0 || y < 0 || x >= length (head grid) || y >= length grid = 0
  | otherwise = (grid !! y) !! x

-- how many alive cells around given cell
countNeighbors :: Grid -> Int -> Int -> Int
countNeighbors grid x y = sum neighbors
  where
    neighbors = [select (x + j) (y + k) | j <- [-1 .. 1], k <- [-1 .. 1]]
    select xj yk
      | xj == x && yk == y = 0
      | otherwise = getCell grid xj yk

-- is cell alive or dead next gen
cellLogic :: Grid -> Int -> Int -> Int
cellLogic grid x y =
  let count = countNeighbors grid x y
      cell = getCell grid x y
   in if count < 2 || count > 3
        then 0 -- dead (under or over pop)
        else
          if (cell /= 1) && count == 3
            then 1 -- (dead and repopulated)
            else cell -- if alive, still alive; if dead, still dead

nextGen :: Grid -> Grid
nextGen grid =
  [ [cellLogic grid x y | x <- [0 .. length (head grid) - 1]]
    | y <- [0 .. length grid - 1]
  ]

stimulateGens :: Grid -> Int -> Grid
stimulateGens grid 0 = grid
stimulateGens grid n = stimulateGens (nextGen grid) (n - 1)

-- Turn Game Objects to String for Printing and Debugging
-- string to then print
showGrid :: Grid -> String
showGrid grid = unlines [show row | row <- grid]

showGens :: [Grid] -> String
showGens [] = "end"
showGens (g : gs) = showGrid g ++ "\n" ++ showGens gs

stimShowGens :: Grid -> Int -> [Grid]
stimShowGens grid 0 = []
stimShowGens grid n =
  let next = nextGen grid
   in next : stimShowGens next (n - 1)

-- MANUAL GRID EDITS

-- row 0 is first row
-- if idx < 0, newRow is appended above first row
-- if idx >= height, newRow is appended below last row
-- will grow the grid size
-- assumes that newRow has correct dimensions (matches column width)
-- if above assumption is broken, no crash, grid will just be malformed
setRow :: Int -> [Int] -> Grid -> Grid
setRow idx newRow game = top ++ [newRow] ++ bottom
  where
    top = take idx game
    bottom = drop (idx+1) game

-- same assumptions as setRow
-- but if newCol given is not same height as game,
-- new game will have height = min of the newCol and game
setCol :: Int -> [Int] -> Grid -> Grid
setCol _ [] _ = []
setCol _ _ [] = []
setCol idx (c : col) (row : rest) =
  let left = take idx row
      right = drop (idx+1) row
   in (left ++ [c] ++ right) : setCol idx col rest

-- for ease, but if not used, then remove
-- assume correct dimensions as above

-- append new row to bottom num
appendRow :: [Int] -> Grid -> Grid
appendRow newRow game = setRow (length game) newRow game

-- append new column to right
appendCol :: [Int] -> Grid -> Grid
appendCol newCol game = setCol (length (head game)) newCol game

-- add empty rows/cols num times
-- num <= 0 then repeats same grid
appendEmptyRows :: Int -> Grid -> Grid
appendEmptyRows num game = addRows num game
  where
    emptyRow = replicate (length (head game)) 0
    -- keep adding empty rows for num iterations
    addRows :: Int -> Grid -> Grid
    addRows n runningGrid
      | n <= 0 = runningGrid
      | otherwise =
          let newGame = appendRow emptyRow runningGrid
          in addRows (n - 1) newGame
appendEmptyCols :: Int -> Grid -> Grid
appendEmptyCols num game = addCols num game
  where
    emptyCol = replicate (length game) 0
    -- keep adding empty cols for num iterations
    addCols :: Int -> Grid -> Grid
    addCols n runningGrid
      | n <= 0 = runningGrid
      | otherwise =
          let newGame = appendCol emptyCol runningGrid
          in addCols (n - 1) newGame


-- set a new value (0/1) at specified cell
-- if not dimensions off grid dims, i don't think it crashes, just becomes malformed
setCell :: Grid -> Int -> Int -> Int -> Grid
setCell game x y newVal =
  let row = (game !! y)
      left = take x row
      right = drop (x+1) row
      newRow = left ++ [newVal] ++ right
   in setRow y newRow game

-- flip alive/dead status of cell
flipCell :: Int -> Int -> Grid -> Grid
flipCell x y grid
  | getCell grid x y == 0 = setCell grid x y 1
  | otherwise = setCell grid x y 0

-- GRID DIMENSION CHANGES

-- make grid a smaller size if need
downsizeGrid :: Grid -> Int -> Int -> Grid
downsizeGrid grid newHeight newWidth
    | newHeight >= length grid && newWidth >= length (head grid) = grid -- downsize not needed
    | otherwise = takeCols newHeight grid where
        -- go through newHeight amount of rows and make it the newWidth length
        takeCols _ [] = []
        takeCols 0 _ = []
        takeCols idx (row : rest) = take newWidth row : takeCols (idx - 1) rest

-- change dimensions of grid, if smaller, leave out cells
-- if bigger, make new dead cells
resizeGrid :: Int -> Int -> Grid -> Grid
resizeGrid newHeight newWidth grid = 
    let oldHeight = length grid
        oldWidth = length (head grid)
    in downsizeGrid (appendEmptyRows (newHeight - oldHeight) -- add new rows if need
    (appendEmptyCols (newWidth - oldWidth) grid)) -- add new cols if need
    newHeight newWidth -- downsize if need

-- turn a 1d strip of dead/alive cells into a rectangular grid 
make2D :: [Int] -> Grid
make2D strip = takeCols strip where
        l = length strip
        newWidth = floor (sqrt (fromIntegral l))
        -- take newWidth size rows out of strip iteratively
        takeCols s 
                -- pad out last row to same width as rest if need
            | length s <= newWidth = [s ++ replicate (newWidth - length s) 1]
            | otherwise = newRow : takeCols rest where
                newRow = take newWidth s
                rest = drop newWidth s