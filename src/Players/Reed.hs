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
                      | otherwise = minimaxAction b ps command r

                reedY b ps command r
                      | validWallAction (Game b ps) yLeftWall  = Just (Place yLeftWall)
                      | validWallAction (Game b ps) yRightWall = Just (Place yRightWall)
                      | otherwise = minimaxAction b ps command r


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
