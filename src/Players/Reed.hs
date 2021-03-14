{-
    Module: Reed.

    *** PART III (10 pt) ***

    Define a player that uses teh Reed opening and play against it. Is the Reed opening a good 
    opening? Write your answers in Reed.txt.
-}
module Players.Reed where

import Types
import Action
import Game
import Players.Minimax

xLeftWall :: Wall
xLeftWall = wallTop ('c',3)

xRightWall :: Wall
xRightWall = wallTop ('f',3)

yLeftWall :: Wall
yLeftWall = wallTop ('c',6)

yRightWall :: Wall
yRightWall = wallTop ('f',6)

-- Create a player that starts with the Reed opening. After that, you may use your minimax action or
-- the given action for DumbPlayer. 
-- [Hint 1: Use the variable 'turn' in Player.]
-- [Hint 2: Use 'wallTop' to get the walls you need.]
-- [Hint 3: Don't forget to check that the action is valid using 'validWallAction'.]
reedPlayerAction :: Board -> [Player] -> String -> Int -> Maybe Action
reedPlayerAction b ps command r
              | name (head ps) == "X" = reedX b ps command r
              | otherwise = reedY b ps command r
              where
                reedX b ps command r
                      | validWallAction (Game b ps) xLeftWall  = Just (Place xLeftWall)
                      | validWallAction (Game b ps) xRightWall = Just (Place xRightWall)
                      | otherwise = minimaxActionReed b ps command r

                reedY b ps command r
                      | validWallAction (Game b ps) yLeftWall  = Just (Place yLeftWall)
                      | validWallAction (Game b ps) yRightWall = Just (Place yRightWall)
                      | otherwise = minimaxActionReed b ps command r


depth' :: Int
depth' = 5
--depth' = 4
--depth' = 3
--depth' = 2
--depth' = 1


breadth' :: Int
breadth' = 10

minimax' :: Int -> Int -> Game -> Action
minimax' b d g =
  (minimaxFromTree .
  --minimaxABFromTree .
  pruneBreadth b .
  highFirst .
  evalTree .
  pruneDepth d .
  generateGameTree) g


-- Given a game state, calls minimax and returns an action.
minimaxActionReed :: Board -> [Player] -> String -> Int -> Maybe Action
minimaxActionReed b ps _ r = let g = Game b ps in minimaxActionReed' g (minimax g)
    where 
        -- Goes through the list of actions until it finds a valid one. 
        minimaxActionReed' :: Game -> Action -> Maybe Action
        minimaxActionReed' g' (Move s)
            | validStepAction g' s = Just (Move s)
            | otherwise = error "Minimax chose an invalid Move action."
        minimaxActionReed' g' (Place w)
            | validWallAction g' w = Just (Place w)
            | otherwise = error "Minimax chose an invalid Place Wall action."
        minimaxActionReed' g' Jump
            | fst (validJumpAction g') = Just Jump
            | otherwise = error "Minimax chose an invalid Jump action"


-- We build a Reed player from a name, a starting cell, a number of walls, an array of winning
-- positions and 'commandToAction'.
makeReedPlayer :: String -> Cell -> Int -> [Cell] -> Player
makeReedPlayer n c rws wps = Player {
    name = n,
    turn = 1,
    currentCell = c, 
    remainingWalls = rws,
    winningPositions = wps,
    isHuman = False,
    chooseAction = reedPlayerAction } 
