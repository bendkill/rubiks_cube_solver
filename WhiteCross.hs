module WhiteCross (solve,
                   whiteCross, whiteEdge,
                   whiteCorners,
                   middleEdges,
                   yellowCross,
                   yellowCorners,
                   posYellowCorners,
                   posYellowEdges
                   ) where

import Prelude hiding (break)
import Cube
import Control.Monad.State
import Data.Maybe

{- algorithms -}

-- whitecross
-- input faces of edge to be flipped
flipEdge :: Face -> Face -> CubeState
flipEdge a b = do
  break
  rot CCW b $ oppositeRot (Rot b)
  rot CW  a $ Rot a
  rot CCW c $ oppositeRot (Rot c)
  rot CCW a $ oppositeRot (Rot a)
  where c = rotFace b a CW

-- white corners
moveCornerUp :: Face -> Face -> CubeState
moveCornerUp a b = do
  break
  let (l,r) = fixLeftRight a b
  rot CCW r $ oppositeRot (Rot r)
  d'
  rot CW  r $ Rot r
  d

moveCornerDown :: Face -> Face -> CubeState
moveCornerDown a b = do
  let (_,r) = fixLeftRight a b
  break
  rot CCW r $ oppositeRot (Rot r)
  d'
  rot CW  r $ Rot r

-- middle edges
bottomEdgeCW :: Face -> CubeState
bottomEdgeCW a = do
  (cube,_) <- get
  let lf = getLeftFace a
  break
  d
  rot CW lf $ Rot lf
  d'
  rot CCW lf $ oppositeRot (Rot lf)
  d'
  rot CCW a $ oppositeRot (Rot a)
  d
  rot CW a $ Rot a

bottomEdgeCCW :: Face -> CubeState
bottomEdgeCCW a = do
  (cube,_) <- get
  let rf = getRightFace a
  break
  d'
  rot CCW rf $ oppositeRot (Rot rf)
  d
  rot CW rf $ Rot rf
  d
  rot CW a $ Rot a
  d'
  rot CCW a $ oppositeRot (Rot a)

-- yellow cross
hasYellowSpots :: [Face] -> Cube -> Bool
hasYellowSpots as cube =
  and $ map (\a -> lookupFaceAt (Edge D a, D) cube == Just D) as

hasYellowCross, hasYellowBend, hasYellowLine :: Cube -> Bool
hasYellowCross = hasYellowSpots [F,R,B,L]
hasYellowBend  = hasYellowSpots [B,R]
hasYellowLine  = hasYellowSpots [L,R]

doYellowCenter, doYellowBend, doYellowLine :: CubeState
doYellowCenter = break >> f >> d >> l >> d' >> l' >> f'
doYellowBend   = doYellowCenter
doYellowLine   = break >> f >> l >> d >> l' >> d' >> f'

-- yellow corners
doYellowCorner :: Face -> Face -> CubeState
doYellowCorner a b = do
  let (f,r) = fixLeftRight a b
      lf = oppositeFace r
      l  = rot CW lf $ Rot lf
      l' = rot CCW lf $ oppositeRot (Rot lf)
  break
  l; d; l'; d; l; d; d; l'

-- positioning yellow Corners correctly
switchDownCorners :: Face -> CubeState
switchDownCorners ff = do
  let
    f  = rot CW ff $ Rot ff
    f' = rot CCW ff $ oppositeRot (Rot ff)
    bf = oppositeFace ff
    b  = rot CW  bf $ Rot bf
    lf = getLeftFace ff
    l  = rot CW  lf $ Rot lf
    l' = rot CCW lf $ oppositeRot (Rot lf)
  break
  l'; f; l'; b; b; l; f'; l'; b; b; l; l; d'

-- positioning yellowEdges correctly
cycleDownEdgesExcept :: Direction -> Face -> CubeState
cycleDownEdgesExcept dir bf = do
  let
    ff = oppositeFace bf
    f  = rot CW  ff $ Rot ff
    rf = getRightFace ff
    r  = rot CW  rf $ Rot rf
    r' = rot CCW rf $ oppositeRot (Rot rf)
    lf = getLeftFace ff
    l  = rot CW  lf $ Rot lf
    l' = rot CCW lf $ oppositeRot (Rot lf)
  break
  case dir of
    CW  -> do {f; f; d; r; l'; f; f; r'; l; d; f; f}
    CCW -> do {f; f; d'; r; l'; f; f; r'; l; d'; f; f}

{- solving for the white (Up) cross -}

whiteEdge :: Face -> CubeState
whiteEdge a = do
  (cube,_) <- get
  let blockT = findBlock (Edge a U) cube
      x = case getPositionOf blockT of
        Edge x U -> x
        Edge U x -> x
        _        -> error $ "whiteEdge found wrong block: " ++ show blockT
      go
        | isCorrect blockT       = return ()
        | blockT `isAt` Edge a U = flipEdge U a     >> whiteEdge a
        | blockT `isOn` U        = rot CW x (Rot x) >> whiteEdge a
        | blockT `isOn` a        = rot CW a (Rot a) >> whiteEdge a
        | blockT `isOn` D        = d                >> whiteEdge a
        | blockT `isOn` oppositeFace a =
          rot CW (oppositeFace a) (Rot $ oppositeFace a)          >> whiteEdge a
        | otherwise = error $ "whiteEdge, no case match: " ++ show blockT
  go

checkWhiteCross :: Cube -> Maybe Face
checkWhiteCross cube
  | check F = Just F
  | check R = Just R
  | check B = Just B
  | check L = Just L
  | otherwise = Nothing
  where check a = not . isCorrect $ findBlock (Edge a U) cube

whiteCross :: CubeState
whiteCross = do
  (cube,_) <- get
  let mFace = checkWhiteCross cube
  case mFace of
    Just a  -> whiteEdge a >> whiteCross
    Nothing -> return ()

{- solve the white (Up) corners -}

whiteCorner :: Face -> Face -> CubeState
whiteCorner a b = do
  (cube,_) <- get
  let blockT = findBlock (Corner a b U) cube
      (x,y) = case getPositionOf blockT of
        Corner x y U -> (x,y)
        Corner x U y -> (x,y)
        Corner U x y -> (x,y)
        _ -> error $ "whiteCorner found wrong block: " ++ show blockT
      go
        | isCorrect blockT           = return ()
        | blockT `isAt` Corner a b U = moveCornerUp a b   >> whiteCorner a b
        | blockT `isAt` Corner a b D = moveCornerUp a b   >> whiteCorner a b
        | blockT `isOn` U            = moveCornerDown x y >> whiteCorner a b
        | blockT `isOn` D            = d                  >> whiteCorner a b
        | otherwise = error $ "whiteCorner, no case match: " ++ show blockT
  go

whiteCorners :: CubeState
whiteCorners = do
  whiteCorner F R
  break
  whiteCorner R B
  break
  whiteCorner B L
  break
  whiteCorner L F

{- solve the middle layer -}

middleEdge :: Face -> Face -> CubeState
middleEdge a b = do
  (cube,_) <- get
  let blockT = findBlock (Edge a b) cube
      (l,r)  = fixLeftRight a b
      go
        | isCorrect blockT       = return ()
        | blockT `isAt` Edge l r = bottomEdgeCCW l >> middleEdge l r
        | blockT `isAt` Edge l D = bottomEdgeCCW l >> middleEdge l r
        | blockT `isAt` Edge D r = bottomEdgeCW  r >> middleEdge l r
        | blockT `isOn` D        = d               >> middleEdge l r
        | blockT `isAt` Edge l (oppositeFace r) = bottomEdgeCW  l >> middleEdge l r
        | blockT `isAt` Edge (oppositeFace l) r = bottomEdgeCCW r >> middleEdge l r
        | blockT `isAt` Edge (oppositeFace l) (oppositeFace r) =
          bottomEdgeCCW (oppositeFace l) >> middleEdge l r
        | otherwise = error $ "middleEdge, no case match: " ++ show blockT
  go

middleEdges :: CubeState
middleEdges = do
  middleEdge F R
  middleEdge R B
  middleEdge B L
  middleEdge L F

{- solve the yellow (Down) cross -}

yellowCross :: CubeState
yellowCross = do
  (cube,_) <- get
  let
    isYellowEdge blockT = blockT `isCorrectOn` D && isEdge blockT
    go
      | hasYellowCross cube                   = return ()
      | hasYellowBend  cube                   = doYellowBend   >> yellowCross
      | hasYellowLine  cube                   = doYellowLine   >> yellowCross
      | countBlocksWith isYellowEdge cube < 2 = doYellowCenter >> yellowCross
      | otherwise                             = d              >> yellowCross
  go

{- solve the yellow (Down) corners -}

-- assumes block is on Down, and a Corner
hasYellowOnRight, hasYellowOnLeft, hasYellowOnDown :: Maybe BlockT -> Bool
hasYellowOnRight (Just blockT) = getDesiredFace t == D
  where (_,t) = uncurry fixLeftRightTiles $
                (\[t0,t1] -> (t0,t1)) $ getTilesExcept D blockT
hasYellowOnLeft (Just blockT) = getDesiredFace t == D
  where (t,_) = uncurry fixLeftRightTiles $
                (\[t0,t1] -> (t0,t1)) $ getTilesExcept D blockT
hasYellowOnDown (Just blockT) = lookupFaceOn D blockT == Just D

doYellowOnRight, doYellowOnDown, doYellowOnLeft :: CubeState
doYellowOnRight = do
  (cube,_) <- get
  let go
        | hasYellowOnRight $ lookupBlockAt (Corner D F R) cube = doYellowCorner F R
        | hasYellowOnRight $ lookupBlockAt (Corner D R B) cube = doYellowCorner R B
        | hasYellowOnRight $ lookupBlockAt (Corner D B L) cube = doYellowCorner B L
        | hasYellowOnRight $ lookupBlockAt (Corner D L F) cube = doYellowCorner L F
        | otherwise = error $ "no Yellow on Right: " ++ show cube
  go
doYellowOnDown = do
  (cube,_) <- get
  let go
        | hasYellowOnDown $ lookupBlockAt (Corner D F R) cube = doYellowCorner F R
        | hasYellowOnDown $ lookupBlockAt (Corner D R B) cube = doYellowCorner R B
        | hasYellowOnDown $ lookupBlockAt (Corner D B L) cube = doYellowCorner B L
        | hasYellowOnDown $ lookupBlockAt (Corner D L F) cube = doYellowCorner L F
        | otherwise = error $ "no Yellow on Down: " ++ show cube
  go
doYellowOnLeft = do
  (cube,_) <- get
  let go
        | hasYellowOnLeft $ lookupBlockAt (Corner D F R) cube = doYellowCorner F R
        | hasYellowOnLeft $ lookupBlockAt (Corner D R B) cube = doYellowCorner R B
        | hasYellowOnLeft $ lookupBlockAt (Corner D B L) cube = doYellowCorner B L
        | hasYellowOnLeft $ lookupBlockAt (Corner D L F) cube = doYellowCorner L F
        | otherwise = error $ "no Yellow on Left: " ++ show cube
  go

yellowCorners :: CubeState
yellowCorners = do
  (cube,_) <- get
  let
    numCorrect = countCorrectOn D cube
    go
        | numCorrect == 8 = return ()
        | numCorrect == 4 = doYellowOnRight >> yellowCorners
        | numCorrect == 5 = doYellowOnDown  >> yellowCorners
        | numCorrect >= 6 = doYellowOnLeft  >> yellowCorners
        | otherwise       = error $ "yellow cross not solved: " ++ show cube
  go

{- position yellow (Down) corners correctly -}

checkAllDownCorners :: Cube -> Bool
checkAllDownCorners cube = and $ map
  (\(a,b) -> isCorrect . fromJust $ lookupBlockAt (Corner D a b) cube)
  [(F,R),(R,B),(B,L),(L,F)]

checkDownCornersOn :: Face -> Cube -> Bool
checkDownCornersOn f cube = isCorrect (fromJust (lookupBlockAt (Corner D f l) cube)) &&
                            isCorrect (fromJust (lookupBlockAt (Corner D f r) cube))
  where
    l = getLeftFace f
    r = getRightFace f

checkDiagonalCorners :: Cube -> Bool
checkDiagonalCorners cube =
  isCorrect fr && isCorrect bl ||
  isCorrect rb && isCorrect lf
  where
    fr = fromJust $ lookupBlockAt (Corner D F R) cube
    rb = fromJust $ lookupBlockAt (Corner D R B) cube
    bl = fromJust $ lookupBlockAt (Corner D B L) cube
    lf = fromJust $ lookupBlockAt (Corner D L F) cube

posYellowCorners :: CubeState
posYellowCorners = do
  (cube,_) <- get
  let
    go
      | checkAllDownCorners  cube = return ()
      | checkDownCornersOn F cube = switchDownCorners B >> posYellowCorners
      | checkDownCornersOn R cube = switchDownCorners L >> posYellowCorners
      | checkDownCornersOn B cube = switchDownCorners F >> posYellowCorners
      | checkDownCornersOn L cube = switchDownCorners R >> posYellowCorners
      | checkDiagonalCorners cube = switchDownCorners F >> posYellowCorners
      | otherwise                 = d                   >> posYellowCorners
  go

{- position yellow (Down) edges correctly -}

posYellowEdges :: CubeState
posYellowEdges = do
  ((Cube blockTs),_) <- get
  let
    correctBlocks = filter (\blockT -> blockT `isOn` D &&
                                       isEdge blockT &&
                                       isCorrect blockT) blockTs
  case correctBlocks of
    [_,_,_,_] -> return ()
    [Edge (Tile D D) (Tile bf _)] -> cycleDownEdgesExcept CW bf >> posYellowEdges
    [Edge (Tile bf _) (Tile D D)] -> cycleDownEdgesExcept CW bf >> posYellowEdges
    _                             -> cycleDownEdgesExcept CW F  >> posYellowEdges


{- solve everything -}

solve :: CubeState
solve = do
  whiteCross
  whiteCorners
  middleEdges
  yellowCross
  yellowCorners
  posYellowCorners
  posYellowEdges