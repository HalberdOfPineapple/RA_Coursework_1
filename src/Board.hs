{-
    Module: Board.

    Checks involving the board and machinery for "placing a wall".
-}
module Board where 

import Data.Graph
import Data.List
import Data.Array

import Types
import Cell
import Action

{-
    Checking if a step is valid and using it to get the reachable cells from a given cell.
-}

-- Check if step is contained in the board, i.e. is one of the edges.
validStep :: Board -> Step -> Bool 
validStep b mv = (stepToEdge mv) `elem` (edges b) 

-- Cells reachable from a given cell.
reachableCells :: Board -> Cell -> [Cell]
reachableCells b c = filter (\c' -> validStep b (c, c')) (cellsAroundInBoard c)



{-
    Checking if a wall is valid.
-}

-- * Categorized parallel relationships of 2 steps to
-- make modifying the effect of placing walls easier after 
-- introduction of feature of diagonal movement
horizontallyParallelSteps :: Board -> Step -> Step -> Bool
horizontallyParallelSteps b (cs, ce) (cs', ce') =
   isVerticallyAdjacent cs cs' && isVerticallyAdjacent ce ce' &&
   isHorizontallyAdjacent cs ce && isHorizontallyAdjacent cs' ce'
   
verticallyParallelSteps :: Board -> Step -> Step -> Bool
verticallyParallelSteps b (cs, ce) (cs', ce') =
        isHorizontallyAdjacent cs cs' && isHorizontallyAdjacent ce ce' &&
        isVerticallyAdjacent cs ce && isVerticallyAdjacent cs' ce'

 -- Check if the steps representing the wall are parallel.
 -- * Modified with the 2 newly added functions describing 2 different parallel relations
parallelSteps :: Board -> Step -> Step -> Bool
parallelSteps b s s' = horizontallyParallelSteps b s s' || verticallyParallelSteps b s s'

-- Check that no wall has been placed that could interfere with the wall we want to place.
noCrossingWalls :: Board -> Step -> Step -> Bool
noCrossingWalls b (cs, ce) (cs', ce') = (validStep b (cs, cs')) && (validStep b (ce, ce'))

-- Check if the edges corresponding to the wall we want to remove are in the board.
validWallSteps :: Board -> Wall -> Bool 
validWallSteps b (s, s') = (validStep b s) && (validStep b s')


-- If the three conditions above are satisfied, it is a valid wall.
validWall :: Board -> Wall -> Bool 
validWall b w@(s, s') = 
    (validWallSteps b w) && (parallelSteps b s s') && (noCrossingWalls b s s')

{-
    Action of placing a wall.
-}

-- Remove a step (used for placing walls).
removeStep :: Board -> Step -> Board 
removeStep b s = 
    let (cI, cI') = stepToEdge s in
    b//[(cI, delete cI' (b!cI)), (cI', delete cI (b!cI'))]

-- * Remove a list of steps (used for placing walls)
removeSteps :: Board -> [Step] -> Board
removeSteps b [] = b
removeSteps b (s:ss)
          | validStep b s = removeSteps (removeStep b s) ss
          | otherwise = removeSteps b ss

-- * List of steps to be removed when original 2 steps are vertically parallel
verticalMovingList :: [Step] -> [Step]
verticalMovingList ss = ss ++ concat [[(a,cellLeft b),(a,cellRight b),(cellLeft a,b),(cellRight a,b)] | (a,b) <- ss]

-- * List of steps to be removed when original 2 steps are horizontally parallel
horizontalMovingList :: [Step] -> [Step]
horizontalMovingList ss = ss ++ concat [[(a,cellTop b),(a,cellBottom b),(cellTop a,b),(cellBottom a,b)] | (a,b) <- ss]

-- Placing a wall is the same as removing the two steps that represent it.
-- * Now Placing a wall requires removing more steps depending on the type of parallel relationship
placeWall :: Board -> Wall -> Board
placeWall b (s,s')
      | verticallyParallelSteps b s s' = removeSteps b (verticalMovingList [s,s'])
      | otherwise = removeSteps b (horizontalMovingList [s,s'])
