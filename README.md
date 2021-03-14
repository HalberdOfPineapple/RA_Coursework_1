# Quoridor in Haskell 

### Installation instructions 

You need GHC, the Cabal build system and the Stack tool. See [https://www.haskell.org/platform/](https://www.haskell.org/platform/). 

### Playing the game 

The easiest way to play the game is to go to the `src` directory, run `ghci Main` and execute the `main` function.

### Run the tests 

There are two test suites:
* Basic tests (`stack test :basic-tests`).
* Minimax tests (`stack test :minimax-tests`).

### About implementation of Minimax Algorihtm
 - Here I used the ordering which swaps `highFirst` and `lowFirst` level by level 
 so the `orderingTree` test might not be passed
  - There are 2 versions of utilities function in my `Minimax.hs`. That is because I thought the utility of a game 
should be different from different player's view, which needs the information of the current player in the 
root state. So I designed another utility function `utility'` which takes the name of player as input
in the game operated by Minimax player and for different player sides, the sign of the function is opposite to each other


### About Reed player
- Implement the reed opening by 
    1. Firstly Checking whether placing the given walls are valid. If so, place 
the walls without considering anything else
    2. After placing the given walls, use minimax algorithms to continue playing
- Copyed the algorithms from already-implemented module `Minimax.hs` for independently 
changing the search depth and breadth.

### About Extensions
#### Feature : Diagonal Move
  For the extension part, I added a new feature which enables the players to move diagonally.
The modification includes:
 - the setting of the game **board** (Graph type):
    1. The initialization of edges includes edges between current cells and their diagonally adjacent neighbors (in `Main.hs`, which
    is helped by introducing a new relation `isDiagonal` in `Cell.hs` and )
    2. The effect of placing walls now will also block diagonal edges (changes made in `Board.hs`,
     where I modified the parallel relationships to facilitate removing edges and the steps removed are expanded to at
      most 8 edges at one time)
 - `canMove` function in `Player.hs` which is also used to determine whether a move is valid:
    1. added condition `isDiagonal` to `canMove` function to allow diagonal steps
 - the range of reachable cells for the current one is modified to include the diagonal neighbors
    1. modified `cellsAroundInBoard` to include diagonal neighbors by adding a list of functions to get diagonal neighbors of a given cell
    , which automatically expand the range of `reachableCell`
    
#### Feature: Jump over the opponent
  I also added another feature which allows the current player to jump across its opponent 
following the direction from it to its opponent when they are adjacent. The modification includes:
- Introduced a new `Action` value called `Jump`
-  Changed the `commandToAction` function in `Human.hs` enabling the program to parse 
    command 'jump' to return a `Jump` Action
-  For modification to `performAction g Jump` in `Game.hs`:
    1. Added `validJumpAction`, given the current game information, to return tuple containing
   boolean value about whether `Jump` is available and the destination Cell when available
    2. Added `isValidJump` to determine whether the actual jump is available considering the edges and board range 
    3. Added `validJump` to fit function `validActions` but only be considered when the 2 players are adjacent
    to reduce computation cost
    4. (in `Player.hs`) Added `jumpTo` function to actually do the operation moving the player to the destination
    cell returned by `validJumpAction`, which is used in `performAction`
- Modified `minimaxAction` in `Minimax.hs` and `Reed.hs` to include this change
