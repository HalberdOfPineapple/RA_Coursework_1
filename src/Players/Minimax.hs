{-
    Module: Minimax.

    *** PART I (60pt) and PART II (10pt) *** 
-}
module Players.Minimax where 

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

{-
    StateTree util.
-}

-- Map a function through the nodes of the tree.
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

{-
    Result util.
-}

-- Negating the result is simply negating the score. You may ignore this although it may be useful
-- to implement the minimax algorithm.
negResult :: Result -> Result
negResult (Result x as) = Result (-x) as

{- 
    *** Part I.a (10pt) ***

    First, we will generate a tree containing all the possible game states.
-}

-- Given a game, return a tree that encodes all the possible future game states.
-- [Hint: Use 'validActions' and 'performAction'.]
-- [Note: To speed things up, you may want to, at this stage, heuristically select which actions are 
--  more relevant. In particular, you probably don't want to consider every single possible wall.]
generateGameTree :: Game -> GameTree
generateGameTree b =
  StateTree b [(a, generateGameTree (fromJust (performAction b a))) | a <- validActions b, isJust (performAction b a)]

{-
    *** PART I.b (5pt) ***

    Re-order the tree so that when traversed by the minimax algorithm, when it traverses the 
    branches at each node, finds either the higher scores or the lower scores first, depending on
    the depth of the tree.
-}

-- Higher scoring nodes go first.
--highFirst :: (Ord v) => StateTree v a -> StateTree v a
--highFirst (StateTree v []) = StateTree v []
--highFirst (StateTree v ts) = StateTree v (sortBy treeCompare (map treeRec ts))
--                      where
--                        treeCompare :: (Ord v) => (a,StateTree v a) -> (a,StateTree v a) -> Ordering
--                        treeCompare (_ ,StateTree v _) (_ , StateTree v' _) = v' `compare` v
--
--                        treeRec :: (Ord v) => (a,StateTree v a) -> (a,StateTree v a)
--                        treeRec (a ,StateTree v ts)  = (a ,highFirst (StateTree v ts))

highFirst :: (Ord v) => StateTree v a -> StateTree v a
highFirst (StateTree v []) = StateTree v []
highFirst (StateTree v ts) = StateTree v (sortBy treeCompare (map treeRec ts))
                      where
                        treeCompare :: (Ord v) => (a,StateTree v a) -> (a,StateTree v a) -> Ordering
                        treeCompare (_ ,StateTree v ts) (_ , StateTree v' ts') = v' `compare` v

                        treeRec :: (Ord v) => (a,StateTree v a) -> (a,StateTree v a)
                        treeRec (a ,StateTree v ts)  = (a ,lowFirst (StateTree v ts))


-- Lower scoring nodes go first.
-- [Hint: You should use 'highFirst'.]
lowFirst :: (Ord v) => StateTree v a -> StateTree v a
lowFirst (StateTree v []) = StateTree v []
lowFirst (StateTree v ts) = StateTree v (sortBy treeCompare (map treeRec ts))
                    where
                         treeCompare :: (Ord v) => (a,StateTree v a) -> (a,StateTree v a) -> Ordering
                         treeCompare (_ ,StateTree v ts) (_ , StateTree v' ts') = v `compare` v'

                         treeRec :: (Ord v) => (a,StateTree v a) -> (a,StateTree v a)
                         treeRec (a ,StateTree v ts)  = (a ,highFirst (StateTree v ts))


{-
    *** Part I.c (5pt) ***

    We don't want to look at all possible future game states as that would consume too much time and
    memory. Instead, we will only look a given number of steps into the future. Formally, the future
    game states are encoded in a tree, so we need a function that reduces the depth of a tree.
-}

-- Given a depth and a tree, return the same tree but cutting off the branches when the depth is 
-- exceeded. 
-- [Hint: You may want to use guards and recursion.]
pruneDepth :: Int -> StateTree v a -> StateTree v a
pruneDepth d (StateTree v ts)
        | d == 0 = StateTree v []
        | length ts == 0 = StateTree v ts
        | otherwise = StateTree v [(a,pruneDepth (d-1) t) | (a,t) <- ts]

{-
    *** Part I.d (5pt) ***

    Similarly, we can also make our tree smaller by not considering all the possible game states at
    a given point. We need a function that reduces the breadth (or width) of a tree.
-}

-- Given a breadth (Int n) and a tree, return the same tree but only keeping the first n branches at
-- every node. 
-- [Hint: Use 'take'.]
pruneBreadth :: Int -> StateTree v a -> StateTree v a
pruneBreadth b (StateTree v ts) = StateTree v (take b [(a,pruneBreadth b t) | (a,t) <- ts])

{-
    *** Part I.e (15pt) ***

    A crucial part of the minimax algorithm is defining a good utility function. It should measure
    how good a game position is for the current player. In our case, a game state should be better
    than another one if the player is closer to its winning positions.
-}

-- Assign a value to each game (from the point of view of the current player).
-- [Hint 1: You may want to calculate the distance between the player's current cell and its winning
--  positions.]
-- [Hint 2: One way would be to use 'reachableCells' repeatedly.]
expandReachable :: [Cell] -> Board -> [Cell]
expandReachable cs b = cs ++ nub (concat [[r | r <- reachableCells b c, r `notElem` cs] | c <- cs])

--utility :: Game -> Int
--utility (Game board ps) =
--  let py = head [p | p <- ps, name p == "Y"]
--      px = head [p | p <- ps, name p == "X"]
--   in util board [currentCell py] (winningPositions py) 0 - util board [currentCell px] (winningPositions px) 0
--  where
--    util :: Board -> [Cell] -> [Cell] -> Int -> Int
--    util b cs ws d
--      | any (`elem` ws) cs = d
--      | otherwise = util b (expandReachable cs b) ws (d + 1)

utility :: Game -> Int
utility (Game board ps) =
  let py = head [p | p <- ps, name p == "Y"]
      px = head [p | p <- ps, name p == "X"]
   in util board [] [currentCell py] (winningPositions py) 0 - util board [] [currentCell px] (winningPositions px) 0
  where
    util :: Board -> [Cell] -> [Cell] -> [Cell] -> Int -> Int
    util b discovered cs ws d
      | any (`elem` ws) cs = d
      | otherwise = util b (discovered ++ cs) (expandReachable cs b) ws (d + 1)
      where
        expanded = concat [[r | r <- reachableCells b c, r `notElem` discovered && r `notElem` cs] | c <- cs]

-- Lifting the utility function to work on trees.
evalTree :: GameTree -> EvalTree
evalTree = mapStateTree utility

{-
    *** Part I.f (20pt) ***

    Finally, we ask you to implement the minimax algorithm. Given an evaluation tree, it should 
    return the a high scoring action (according to the minimax algorithm).
-}

-- Given an evaluation tree (it stores a score in the node and each branch is labelled with the 
-- action that leads to the next child) return a list of actions
-- [Hint 1: Use a helper function to keep track of the highest and lowest scores.]
-- [Hint 2: Use the 'Result' datatype.]
minimaxFromTree :: EvalTree -> Action
minimaxFromTree et = getResult $ aux_max [] et
              where
                aux_max :: [Action] -> EvalTree -> Result
                aux_max as (StateTree x []) = Result x as
                aux_max as (StateTree _ ts) = head [aux_min (as ++ [a]) t | (a,t) <- ts ]
                
                aux_min :: [Action] -> EvalTree -> Result
                aux_min as (StateTree x []) = Result x as
                aux_min as (StateTree _ ts) = head [aux_max (as ++ [a]) t | (a,t) <- ts ] --when high and low are switched from one level to the next, use 'head'
                
                getResult :: Result -> Action
                getResult (Result _ as) = head as
{-
    *** Part II (10pt) ***

    Extension of Part I.e, using alpha-beta pruning. You will need to change the 'minimax' function
    below to make it use this function.
-}

-- Same as above but now use alpha-beta pruning.
-- [Hint 1: Extend the helper function in I.e to keep track of alpha and beta.]
-- [Hint 2: Use the 'Result' datatype.]
posInf = maxBound :: Int

negInf = minBound :: Int

minimaxABFromTree :: String -> EvalTree -> Action
minimaxABFromTree pn et
                | pn == "X" = getResult $ aux_max [] (Result posInf []) (Result negInf []) et
                | otherwise = getResult $ aux_min [] (Result posInf []) (Result negInf []) et
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

{-
    Putting everything together.
-}

-- Given depth for pruning (should be even).
depth :: Int 
--depth = 4
depth = 3

-- Given breadth for pruning.
breadth :: Int 
breadth = 10

-- Function that combines all the different parts implemented in Part I.
minimax :: Game -> Action
minimax (Game b ps) --minimaxFromTree
  | fp == "X" = (minimaxABFromTree fp . pruneBreadth breadth . highFirst . evalTree . pruneDepth depth . generateGameTree) (Game b ps)
  | otherwise = (minimaxABFromTree fp . pruneBreadth breadth . lowFirst  . evalTree . pruneDepth depth . generateGameTree) (Game b ps)
  where
    fp = name (head ps)

-- Given a game state, calls minimax and returns an action.
minimaxAction :: Board -> [Player] -> String -> Int -> Maybe Action
minimaxAction b ps _ r = let g = Game b ps in minimaxAction' g (minimax g)
    where 
        -- Goes through the list of actions until it finds a valid one. 
        minimaxAction' :: Game -> Action -> Maybe Action
        minimaxAction' g' (Move s)
            | validStepAction g' s = Just (Move s)
            | otherwise = error "Minimax chose an invalid action."
        minimaxAction' g' (Place w)
            | validWallAction g' w = Just (Place w)
            | otherwise = error "Minimax chose an invalid action."

-- Make minimaxPlayer in the usual way using 'minimaxAction'.
makeMinimaxPlayer :: String -> Cell -> Int -> [Cell] -> Player
makeMinimaxPlayer n c rws wps = Player {
    name = n,
    turn = 1,
    currentCell = c, 
    remainingWalls = rws,
    winningPositions = wps,
    isHuman = False,
    chooseAction = minimaxAction }
