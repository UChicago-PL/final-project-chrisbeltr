# Conway's Brain of Fuck

We have implemented Conway’s Game of Life in Haskell. Running our project will provide a GUI that lets you set and edit the game grid environment and then run the game of life cell automata ruleset on the game grid. We adopted the original, standard rule set—at each step in time, each cell’s next state is determined as follows:
- Any live cell with fewer than two live neighbours dies (underpopulation)
- Any live cell with two or three live neighbours lives on to the next generation.
- Any live cell with more than three neighbors dies (overpopulation)
- Any dead cell with exactly three live neighbors becomes a live cell (repopulation)

We have also implemented a BrainFuck (BF) parser, such that BF code can be imported and turned into corresponding game grid states. The program can then run the stimulation of the BF-turned-conway-game. At any state of the game grid, the program can turn the grid state into BF code and export it into a BF file.

# How to run
- Set up GHC and Stack if you haven’t already (follow the directions at https://www.haskell.org/ghcup/, specifically checking the option for better integration of stack with GHCup)
- Clone the repo
- Install any dependencies from monomer (https://github.com/fjvallarino/monomer/blob/main/docs/tutorials/00-setup.md#libraries-sdl2-and-glew) 
- Run `stack run cbf`
- This will then bring up the GUI window

# How to use
- The GUI window has a slider to customize the grid dimensions.
- There is a play/pause button to start or stop a continuous game/cell generation.
- There is also a speed slider to customize how fast or slow the generation transitions are shown.
- There is a reset button to make the grid all empty.
- There is a step button that will only move the grid one generation (as opposed to play’s continuous stepping).
- You can press on any cell to manually flip the cell’s dead/alive status.
- You can press space and this will put you in a “painting” mode where any cell you cursor touches (without pressing) will be flipped; press space again to turn this mode off.
- You can press the “Import BrainFuck” button to turn code in `input.bf` into a game grid state.
- You can press the “Export BrainFuck” button to turn game grid state into BF code (alternatively serves as a way to save/store grid states as files).

# Project Structure
Our primary code files are in the `src` folder and contains the following:
- `Main.hs`: the file to run the GUI window and display the game grid and cell generations
- `Logic.hs`: file holding the game logic and functions pertaining the game of life’s rule set, creating next generation, grid creation, editing, and dimensions
  - Main.hs imports Logic.hs to use game logic 
- `Brainfuck.hs`: file holding the code that parses BF code and converts between grid state and BF code
  - Main.hs imports Brainfuck.hs to handle BF importation and exportation
- Brainfuck.hs imports Logic.hs to handle grid creation and grid dimension handling

There are few other notable files outside of `src` and instead exists in the broader repo folder
- `import.bf`: BF code that  the “Import BrainFuck” takes as input
  - The file name itself is hardcoded to the program but the BF code inside can be edited to interact with what the program converts
- `export.bf`: output file of the “Export BrainFuck” button
