{-
    Module: Game.

    Functions used in the game loop to change the game state.
-}
module Game where

import Types
import Constants
import Action
import Board 
import Player
import Cell

{-
    'performAction' and helpers.
-}

-- The current player is the first element in the players list.
currentPlayer :: [Player] -> Player 
currentPlayer = head

-- The previous player is the last element in the players list.
previousPlayer :: [Player] -> Player 
previousPlayer = last

-- The player that just played goes to the back of the list. Used to change turn.
rotatePlayers :: [Player] -> [Player]
rotatePlayers [] = [] 
rotatePlayers (p:ps) = ps ++ [p]

-- A step action is valid if the step is valid and no other player is in the target cell (canMove).
validStepAction :: Game -> Step -> Bool
validStepAction (Game b ps) s = (canMove (currentPlayer ps) ps s) && (validStep b s)

-- Generate all valid steps at a game state.
validSteps :: Game -> [Action]
validSteps g@(Game b ps) = map Move (filter (validStepAction g) steps)
    where  
        steps = let p = currentPlayer ps in makeSteps (currentCell p) (adjacentCells p)

-- A wall action is valid if the wall is valid and player has walls remaining.
validWallAction :: Game -> Wall -> Bool 
validWallAction (Game b ps) w = (hasWallsLeft (currentPlayer ps)) && (validWall b w)

-- Generate all valid walls at a game state.
validWalls :: Game -> [Action]
validWalls g = map Place (filter (validWallAction g) walls)
    where 
        walls = concat [[wallRight c, wallTop c] | c<-[(i, j) | i<-allColumns, j<-allRows]]

-- * Determine whether the actual jump is available considering the valid steps and board range
isValidJump :: Board -> Cell -> Cell -> Cell -> (Bool,Cell)
isValidJump b currC oppoC dest = (cellInBoard dest && validStep b (currC,oppoC) && validStep b (oppoC,dest),dest)

-- * Determine whether there is an available jump action considering the relation ship of 
-- 2 players' positions
validJumpAction :: Game -> (Bool,Cell)
validJumpAction (Game b ps) = validJumpAction' b (currentCell (head ps)) (currentCell (last ps))
          where
            validJumpAction' :: Board -> Cell -> Cell -> (Bool,Cell)
            validJumpAction' b currC oppoC
                | oppoC == cellTop currC = isValidJump b currC oppoC (cellTop oppoC)
                | oppoC == cellBottom currC = isValidJump b currC oppoC (cellBottom oppoC)
                | oppoC == cellLeft currC = isValidJump b currC oppoC (cellLeft oppoC)
                | oppoC == cellRight currC = isValidJump b currC oppoC (cellRight oppoC)
                | otherwise = (False, ('0', 0))

-- * Generate a list including a single Jump or nothing
validJump :: Game -> [Action]
validJump g
  | fst (validJumpAction g) = [Jump]
  | otherwise = []

-- Generate all valid actions at a game state.
-- * Modified to consider Jump action when 2 players are adjacent
validActions :: Game -> [Action]
validActions g@(Game b ps)
  | isAdjacent (currentCell (head ps)) (currentCell (last ps)) = validSteps g ++ validWalls g ++ validJump g
  | otherwise = validSteps g ++ validWalls g

-- Key function. Given a game and an action, checks the validity of the action and applies it to the
-- game, generating a new game.
-- * Modified to perform the Jump action
performAction :: Game -> Action -> Maybe Game
performAction g@(Game b (p:ps)) (Move s)
    | validStepAction g s = 
        Just (Game b (rotatePlayers ((movePlayer (nextTurn p) s):ps)))
    | otherwise = Nothing
performAction g@(Game b (p:ps)) (Place w)
    | validWallAction g w = 
        Just (Game (placeWall b w) (rotatePlayers ((useWall (nextTurn p)):ps)))
    | otherwise = Nothing
performAction g@(Game b (p:ps)) Jump
    | fst (validJumpAction g) =
        Just (Game b (rotatePlayers (nextP : ps)))
    | otherwise = Nothing
       where
            nextP = jumpTo (nextTurn p) (snd (validJumpAction g))