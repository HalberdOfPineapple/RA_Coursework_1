module Draft where

import Data.Maybe
import Data.Graph
import Data.Ord
import Data.Tree
import Data.List
import Data.Array

import Types
import Constants
import Cell
import Action
import Board
import Player
import Game
import Players.Dumb (dumbAction)

----------------------------------------------------------------

mapStateTree :: (v -> w) -> StateTree v a -> StateTree w a
mapStateTree f (StateTree x ts) = StateTree (f x) [(a, mapStateTree f t) | (a, t)<-ts]

-- Calculate the depth of the tree (used to test pruneDepth).
stateTreeDepth :: StateTree v a -> Int
stateTreeDepth (StateTree _ []) = 0
stateTreeDepth (StateTree _ ts) = 1 + (maximum (map (stateTreeDepth . snd) ts))

-- Calculate the breadth of the tree (used to test pruneBreadth).
stateTreeBreadth :: StateTree v a -> Int
stateTreeBreadth (StateTree _ []) = 0
stateTreeBreadth (StateTree _ ts) = max (length ts) (maximum (map (stateTreeBreadth . snd) ts))

------------------------------------------------
--I.b

type TestTree = StateTree Int Int

{-
    Util.
-}

-- Retrieve value stored at the node.
nodeValue :: TestTree -> Int 
nodeValue (StateTree x _) = x

-- First subtree (used to check the second level).
firstSubtree :: TestTree -> TestTree 
firstSubtree (StateTree _ []) = error "No branches"
firstSubtree (StateTree _ (t:_)) = snd t

-- Get value of top branch.
topBranchValue :: TestTree -> Int 
topBranchValue t = nodeValue (firstSubtree t)

testTree :: TestTree 
testTree = StateTree 0 [(3, StateTree 3 ts), (4, StateTree 4 ts), (0, StateTree 0 ts)]
    where 
        ts = [(5, StateTree 5 []), (7, StateTree 7 []), (1, StateTree 1 [])]

getv :: StateTree v a -> v
getv (StateTree v a) = v

highFirst :: (Ord v) => StateTree v a -> StateTree v a
highFirst (StateTree v []) = (StateTree v [])
highFirst (StateTree v ts) = StateTree v (sortBy treeCompare (map treeRec ts))
                      where
                        treeCompare :: (Ord v) => (a,StateTree v a) -> (a,StateTree v a) -> Ordering
                        treeCompare (_ ,StateTree v ts) (_ , StateTree v' ts') = v' `compare` v

                        treeRec :: (Ord v) => (a,StateTree v a) -> (a,StateTree v a)
                        treeRec (a ,StateTree v ts)  = (a ,lowFirst (StateTree v ts))


-- Lower scoring nodes go first.
-- [Hint: You should use 'highFirst'.]
lowFirst :: (Ord v) => StateTree v a -> StateTree v a
lowFirst (StateTree v []) = (StateTree v [])
lowFirst (StateTree v ts) = StateTree v (sortBy treeCompare (map treeRec ts))
                    where
                         treeCompare :: (Ord v) => (a,StateTree v a) -> (a,StateTree v a) -> Ordering
                         treeCompare (_ ,StateTree v ts) (_ , StateTree v' ts') = v `compare` v'

                         treeRec :: (Ord v) => (a,StateTree v a) -> (a,StateTree v a)
                         treeRec (a ,StateTree v ts)  = (a ,highFirst (StateTree v ts))
                         
------------------------------------------------
--I.C
testTree0 :: TestTree
testTree0 = StateTree 6 [] 

-- Tree of depth 1.
testTree1 :: TestTree 
testTree1 = StateTree 8 [(1, testTree0), (3, testTree0)]

-- Tree of depth 2.
testTree2 :: TestTree 
testTree2 = StateTree 4 [(2, testTree1)]

-- Tree of depth 3.
testTree3 :: TestTree 
testTree3 = StateTree 1 [(5, testTree2), (7, testTree2), (1, testTree2)]

pruneDepth :: Int -> StateTree v a -> StateTree v a
pruneDepth d (StateTree v ts)
        | d == 0 = StateTree v []
        | length ts == 0 = StateTree v ts
        | otherwise = StateTree v [(a,pruneDepth (d-1) t) | (a,t) <- ts]
                         
------------------------------------------------
--I.e Utility function

highestRow :: [Cell] -> Int
highestRow cs = maximum [snd c | c <- cs]

lowestRow :: [Cell] -> Int
lowestRow cs = minimum [snd c | c <- cs]

expandReachable :: [Cell] -> Board -> [Cell]
expandReachable cs b = cs ++ (nub (concat [[r | r <- reachableCells b c, not (r `elem` cs)] | c <- cs]))


utilityOriginal :: Game -> Int
utilityOriginal (Game board ps) = util board [currentCell (head ps)] (winningPositions (head ps)) 0
            where
              util :: Board -> [Cell] -> [Cell] -> Int -> Int
              util b cs ws d
                | any (`elem` ws) cs = -d
                | otherwise = util b (expandReachable cs b) ws (d+1)

utilitySecond :: Game -> Int
utilitySecond (Game board ps) =
  let py = head [p | p <- ps, name p == "Y"]
      px = head [p | p <- ps, name p == "X"]
   in util board [currentCell py] (winningPositions py) 0 - util board [currentCell px] (winningPositions px) 0
  where
    util :: Board -> [Cell] -> [Cell] -> Int -> Int
    util b cs ws d
      | any (`elem` ws) cs = d
      | otherwise = util b (expandReachable cs b) ws (d + 1)


utility' :: String -> Game -> Int
utility' pn (Game board ps)
  | pn == "X" =
    util board py [currentCell py] (winningPositions py) 0 currRowY 0 -
    util' board px [currentCell px] (winningPositions px) 0 currRowX 0
  | otherwise =
    util' board px [currentCell px] (winningPositions px) 0 currRowX 0 -
    util board py [currentCell py] (winningPositions py) 0 currRowY 0
  where
    py = head [p | p <- ps, name p == "Y"]
    px = head [p | p <- ps, name p == "X"]
    currRowY = snd (currentCell py)
    currRowX = snd (currentCell px)
      -- util is for going downwards (Player 'Y')
    util :: Board -> Player -> [Cell] -> [Cell] -> Int -> Int -> Int -> Int
    util b p cs ws d lowest counter
      | any (`elem` ws) cs = d
      | counter > boardSize = 2 * boardSize
      | lowest == lr = util b p (expandReachable cs b) ws (d + 1) lowest (counter + 1)
      | otherwise = util b p (expandReachable cs b) ws (d + 1) lr 0
      where
        lr = lowestRow cs
 
      -- util' is for going upwards (Player 'X')
    util' :: Board -> Player -> [Cell] -> [Cell] -> Int -> Int -> Int -> Int
    util' b p cs ws d highest counter
      | any (`elem` ws) cs = d
      | counter > boardSize = 2 * boardSize
      | highest == hr = util' b p (expandReachable cs b) ws (d + 1) highest (counter + 1)
      | otherwise = util' b p (expandReachable cs b) ws (d + 1) hr 0
      where
        hr = highestRow csting the utility function to work on trees.

utility'' :: String -> Game -> Int
utility'' pn (Game board ps)
  | pn == "X" =
    util board py [] [currentCell py] 0 -
    util board px [] [currentCell px] 0
  | otherwise =
    util board px [] [currentCell px] 0 -
    util board py [] [currentCell py] 0
  where
    py = head [p | p <- ps, name p == "Y"]
    px = head [p | p <- ps, name p == "X"]
    
    util :: Board -> Player -> [Cell] -> [Cell] -> Int -> Int
    util b p prev curr d
          | any (`elem` winningPositions p) curr = d
          | prev == curr = 2 * boardSize
          | otherwise = util b p curr (expandReachable curr b) (d + 1)

--utility' :: Game -> Int
--utility' (Game board ps) = let py = head [p | p <- ps, name p == "Y"], px = head [p | p <- ps, name p == "X"] in
--                    (util board [] [currentCell py] (winningPositions py) 0) - (util board [] [currentCell px] (winningPositions px) 0)
--                    where
--                      util :: Board -> [Cell] -> [Cell] -> [Cell] -> Int -> Int
--                      util b discovered cs ws d
--                        | any (`elem` ws) cs =  
--                        | otherwise = util b (discovered ++ cs) (expandReachable cs b) ws (d+1)
--                        where
--                            expanded = concat [[r | r <- reachableCells b c, r `notElem` discovered && r `notElem` cs] | c <- cs]
     
                        
-- Lifting the utility function to work on trees.
evalTree :: GameTree -> EvalTree 
evalTree = mapStateTree utility 

------------------------------------------------
--I.f implementing Minimax
minimaxFromTree :: EvalTree -> Action
minimaxFromTree et = getResult $ aux_max [] et
              where
                aux_max :: [Action] -> EvalTree -> Result
                aux_max as (StateTree x []) = Result x as
                aux_max as (StateTree _ ts) = head [aux_min (as ++ [a]) t | (a,t) <- ts ]
                
                aux_min :: [Action] -> EvalTree -> Result
                aux_min as (StateTree x []) = Result x as
                aux_min as (StateTree _ ts) = last [aux_max (as ++ [a]) t | (a,t) <- ts ]
                
                getResult :: Result -> Action
                getResult (Result _ as) = head as

------------------------------------------------
--Part 2
posInf = maxBound :: Int

negInf = minBound :: Int

minimaxABFromTree :: EvalTree -> Action
minimaxABFromTree et = getResult $ aux_max [] (Result posInf []) (Result negInf []) et
                where
                      aux_max :: [Action] -> Result -> Result -> EvalTree -> Result
                      aux_max as alpha _ (StateTree x []) = Result x as
                      aux_max as alpha beta (StateTree x [t]) = max (aux_min (as ++ [fst t]) alpha beta (snd t)) alpha
                      aux_max as alpha beta (StateTree x (t:ts))
                                                  | alphaCand >= beta = alphaCand -- alphaCand is just the v returned (???)
                                                  | otherwise = aux_max as (max alpha alphaCand) beta (StateTree x ts)
                            where
                                  alphaCand = aux_min (as ++ [fst t]) alpha beta (snd t)

                      aux_min :: [Action] -> Result -> Result -> EvalTree -> Result
                      aux_min as _ beta (StateTree x []) = Result x as
                      aux_min as alpha beta (StateTree x [t]) = min (aux_max (as ++ [fst t]) alpha beta (snd t)) beta
                      aux_min as alpha beta (StateTree x (t:ts))
                                                  | betaCand <= alpha = betaCand
                                                  | otherwise = aux_min as alpha (min beta betaCand) (StateTree x ts)
                              where
                                  betaCand = aux_max (as ++ [fst t]) alpha beta (snd t)


                      getResult :: Result -> Action
                      getResult (Result _ as) = head as

----------------------------------------------
--Reed utilities
xLeftWall :: Wall
xLeftWall = wallTop ('c',3)

xRightWall :: Wall
xRightWall = wallTop ('f',3)

yLeftWall :: Wall
yLeftWall = wallTop ('c',6)

yRightWall :: Wall
yRightWall = wallTop ('f',6)



----------------------------------------------
--Self-test utilities
type TestTreeII = StateTree Int String

abtestTree :: TestTreeII
abtestTree = StateTree 3 [("A1", StateTree 3 st1), ("A2", StateTree 2 st2), ("A3", StateTree 2 st3)]
    where
        st1 = [("A11", StateTree 3 []), ("A12", StateTree 12 []), ("A13", StateTree 8 [])]
        st2 = [("A21", StateTree 2 []), ("A22", StateTree 4 []), ("A13", StateTree 6 [])]
        st3 = [("A11", StateTree 14 []), ("A12", StateTree 5 []), ("A13", StateTree 2 [])]



