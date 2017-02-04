module Cube (Face (F, B, U, D, L, R),
             Tile (Tile), getCurrentFace, getDesiredFace,
             Block (Corner, Edge),
             BlockP, BlockF, BlockT,
             Cube (Cube, getCube),
             Spot,
             getPositionOf,
             isAt,
             lookupBlockAt,
             lookupFaceAt,
             isOn,
             getBlocksOn,
             getBlocksOff,
             findBlock,
             oppositeFace, getLeftFace, getRightFace,
             isLeftOf, isRightOf, fixLeftRight,
             tileIsLeftOf, tileIsRightOf, fixLeftRightTiles,
             checkIfCorrect,
             isCorrect, isCorrectOn,
             isCorner, isEdge,
             hasTile,
             countBlocksWith,
             countColorsOn, countCorrectOn,
             getTilesExcept, lookupFaceOn,
             Direction (CW, CCW),
             rotFace, rotate,
             Rotation (Rot,U',L',F',R',B',D',U2,L2,F2,R2,B2,D2,Null,Break),
             doubleRot, oppositeRot,
             Solution,
             CubeState, runCube, runSteps, rot, break,
             u, l, f, r, b, d,
             u',l',f',r',b',d',
             solvedCube, allPositions
            ) where

import Prelude hiding (break)
import Data.List hiding (break)
import Data.Maybe
import Control.Monad.State

{- Constructing a Cube -}

data Face
  = F                           -- Front
  | B                           -- Back
  | U                           -- Up
  | D                           -- Down
  | L                           -- Left
  | R                           -- Right
  deriving (Eq)

instance Show Face where
  show F = "\x1B[31m◼︎\x1B[0m"   -- Red "◼︎"
  show B = "\x1B[36m◼︎\x1B[0m"   -- Cyan (usually Orange)
  show U = "\x1B[29m◼︎\x1B[0m"   -- White
  show D = "\x1B[33m◼︎\x1B[0m"   -- Yellow
  show L = "\x1B[32m◼︎\x1B[0m"   -- Green
  show R = "\x1B[34m◼︎\x1B[0m"   -- Blue

data Tile = Tile {getCurrentFace :: Face,
                  getDesiredFace :: Face} -- "color"

instance Eq Tile where
  Tile x a == Tile y b = x == y && a == b

instance Show Tile where
  show (Tile a b) = "(" ++ show a ++ "," ++ show b ++ ")"

data Block a
  = Corner a a a
  | Edge a a
  deriving (Show)

type BlockP = Block Face -- gen'l block position on the cube
type BlockF = Block Face -- "colors" of a block, no knowledge of position
type BlockT = Block Tile -- particular block position WITH current colors

instance Eq a => Eq (Block a) where
  Edge _ _     == Corner _ _ _ = False
  Corner _ _ _ == Edge _ _     = False
  Corner a b c == Corner x y z =
    [a,b,c] `elem` permutations [x,y,z]
  Edge   a b   == Edge   x y   =
    [a,b]   `elem` permutations [x,y]

type Spot = (BlockP, Face)

data Cube = Cube {getCube :: [BlockT]}

instance Show Cube where
  show cube = subColorsIn 'c' template $ colors cube where
    faces cube_ = map ((show <$>) . (`lookupFaceAt` cube_)) allSpots
    colors = map (fromMaybe "_") . faces
    template = "\n\
\         c c c\n\
\         c " ++ show U ++ " c\n\
\         c c c\n\
\\n\
\ c c c   c c c   c c c   c c c\n\
\ c " ++ show L ++ " c   c " ++ show F ++ " c  \
\ c " ++ show R ++ " c   c " ++ show B ++ " c\n\
\ c c c   c c c   c c c   c c c\n\
\\n\
\         c c c\n\
\         c " ++ show D ++ " c\n\
\         c c c\n\
\\n"

subColorsIn :: Char -> String -> [String] -> String
subColorsIn _ _ [] = ""
subColorsIn _ "" _ = ""
subColorsIn c (a:as) bs@(s:ss)
  | a == c    = s ++ subColorsIn c as ss
  | otherwise = a :  subColorsIn c as bs

instance Eq Cube where
  a == b = show a == show b

{- examine the blocks a given cube, at a given position -}

getPositionOf :: BlockT -> BlockP
getPositionOf blockT = case blockT of
  Corner (Tile a _) (Tile b _) (Tile c _) -> Corner a b c
  Edge   (Tile a _) (Tile b _)            -> Edge a b

isAt :: BlockT -> BlockP -> Bool
isAt blockT blockP = blockP == getPositionOf blockT

lookupBlockAt :: BlockP -> Cube -> Maybe BlockT
lookupBlockAt blockP (Cube blocks) = case filter (`isAt` blockP) blocks of
  [x] -> Just x
  []  -> Nothing
  _   -> error "multiple blocks at same position"

lookupFaceAt :: Spot -> Cube -> Maybe Face
lookupFaceAt (blockP,face) cube = case blockT of
  Just (Corner (Tile a1 a2) (Tile b1 b2) (Tile c1 c2)) -> lookup face [(a1,a2),
                                                                       (b1,b2),
                                                                       (c1,c2)]
  Just (Edge (Tile a1 a2) (Tile b1 b2)) -> lookup face [(a1,a2),(b1,b2)]
  _ -> Nothing
  where
    blockT = lookupBlockAt blockP cube

{- find a given block on a given cube -}

getFacesOf :: BlockT -> BlockF
getFacesOf blockT = case blockT of
  Corner (Tile _ a) (Tile _ b) (Tile _ c) -> Corner a b c
  Edge   (Tile _ a) (Tile _ b)            -> Edge a b

blockHasFaces :: BlockT -> BlockF -> Bool
blockHasFaces blockT blockF = blockF == getFacesOf blockT

findBlock :: BlockF -> Cube -> BlockT
findBlock blockF (Cube blockTs) = case filter (`blockHasFaces` blockF) blockTs of
  [x] -> x
  []  -> error $ "block not found: " ++ show blockF

{- examine the blocks on a given face -}

isOn :: BlockT -> Face -> Bool
block `isOn` face = case block of
  Corner (Tile a _) (Tile b _) (Tile c _) -> face `elem` [a,b,c]
  Edge   (Tile a _) (Tile b _)            -> face `elem` [a,b]

getBlocksOn :: Face -> Cube -> [BlockT]
getBlocksOn face = filter (`isOn` face) . getCube

getBlocksOff :: Face -> Cube -> [BlockT]
getBlocksOff face = filter (not . (`isOn` face)) . getCube

{- miscellaneous operations on faces -}

oppositeFace :: Face -> Face
oppositeFace a = case a of
  U -> D; D -> U
  L -> R; R -> L
  F -> B; B -> F

getLeftFace, getRightFace :: Face -> Face
getLeftFace  a = rotFace a U CW
getRightFace a = rotFace a U CCW

isLeftOf, isRightOf :: Face -> Face -> Bool
a `isLeftOf`  b = a == getLeftFace b
a `isRightOf` b = a == getRightFace  b

fixLeftRight :: Face -> Face -> (Face,Face)
fixLeftRight a b
  | a `isLeftOf` b  = (a,b)
  | a `isRightOf` b = (b,a)
  | otherwise       = error $ "faces not adjacent: " ++ show a ++ ", " ++ show b

tileIsLeftOf, tileIsRightOf :: Tile -> Tile -> Bool
t0 `tileIsLeftOf`  t1 = getCurrentFace t0 `isLeftOf`  getCurrentFace t1
t0 `tileIsRightOf` t1 = getCurrentFace t0 `isRightOf` getCurrentFace t1

fixLeftRightTiles :: Tile -> Tile -> (Tile,Tile)
fixLeftRightTiles t0 t1
  | t0 `tileIsLeftOf` t1  = (t0,t1)
  | t0 `tileIsRightOf` t1 = (t1,t0)
  | otherwise =
    error $ "tiles not adjacent: " ++ show t0 ++ ", " ++ show t1

{- Miscellaneous operations on BlockTs -}

checkIfCorrect :: BlockF -> Cube -> Bool
checkIfCorrect blockF cube = isCorrect $ findBlock blockF cube

isCorrect :: BlockT -> Bool
isCorrect (Corner (Tile a x) (Tile b y) (Tile c z)) =
  a == x && b == y && c == z
isCorrect (Edge (Tile a x) (Tile b y)) =
  a == x && b == y

isCorrectOn :: BlockT -> Face -> Bool
isCorrectOn blockT face = blockT `hasTile` Tile face face

isCorner, isEdge :: Block a -> Bool
isCorner (Corner _ _ _) = True
isCorner _ = False
isEdge = not . isCorner

hasTile :: BlockT -> Tile -> Bool
Corner a b c `hasTile` tile =
  tile `elem` [a,b,c]
Edge a b `hasTile` tile = tile `elem` [a,b]

lookupFaceOn :: Face -> BlockT -> Maybe Face
lookupFaceOn q blockT@(Corner (Tile a x) (Tile b y) (Tile c z))
  | q == a = Just x
  | q == b = Just y
  | q == c = Just z
  | otherwise = Nothing

getTilesExcept :: Face -> BlockT -> [Tile]
getTilesExcept p (Corner (Tile a x) (Tile b y) (Tile c z)) =
  map (\(a,x) -> Tile a x) $ filter ((/=p) . fst) [(a,x),(b,y),(c,z)]
getTilesExcept p (Edge (Tile a x) (Tile b y)) =
  map (\(a,x) -> Tile a x) $ filter ((/=p) . fst) [(a,x),(b,y)]

{- functions for counting blocks -}

countBlocksWith :: (BlockT -> Bool) -> Cube -> Int
countBlocksWith p (Cube blockTs) = length $ filter p blockTs

-- (color to be counted) -> (face to be counted on) -> Cube -> Occurences
countColorsOn :: Face -> Face -> Cube -> Int
countColorsOn a face cube = countBlocksWith (`hasTile` Tile face a) cube

countCorrectOn :: Face -> Cube -> Int
countCorrectOn a = countColorsOn a a

{- rotation of a given face -}

data Direction = CW | CCW

rotErr = error "illegal rotation"

-- (Face to rotate) -> (Face to rotate about) -> (Resulting Face)
rotFaceCW :: Face -> Face -> Face
rotFaceCW a F = case a of
  L -> U
  U -> R
  R -> D
  D -> L
  _ -> rotErr
rotFaceCW a U = case a of
  L -> B
  B -> R
  R -> F
  F -> L
  _ -> rotErr
rotFaceCW a L = case a of
  U -> F
  F -> D
  D -> B
  B -> U
  _ -> rotErr
rotFaceCW a B = rotFaceCCW a F
rotFaceCW a D = rotFaceCCW a U
rotFaceCW a R = rotFaceCCW a L

rotFaceCCW :: Face -> Face -> Face
rotFaceCCW a F = case a of
  L -> D
  U -> L
  R -> U
  D -> R
  _ -> rotErr
rotFaceCCW a U = case a of
  L -> F
  B -> L
  R -> B
  F -> R
  _ -> rotErr
rotFaceCCW a L = case a of
  U -> B
  F -> U
  D -> F
  B -> D
  _ -> rotErr
rotFaceCCW a B = rotFaceCW a F
rotFaceCCW a D = rotFaceCW a U
rotFaceCCW a R = rotFaceCW a L

rotFace :: Face -> Face -> Direction -> Face
rotFace a face dir = case dir of
  CW  -> rotFaceCW  a face
  CCW -> rotFaceCCW a face

rotOneBlock :: Direction -> Face -> BlockT -> BlockT
rotOneBlock dir face block = case block of
  Corner (Tile a x) (Tile b y) (Tile c z) -> corner
    where corner
            | face == a = Corner (Tile a x)
                                 (Tile (rotFace b face dir) y)
                                 (Tile (rotFace c face dir) z)
            | face == b = Corner (Tile (rotFace a face dir) x)
                                 (Tile b y)
                                 (Tile (rotFace c face dir) z)
            | face == c = Corner (Tile (rotFace a face dir) x)
                                 (Tile (rotFace b face dir) y)
                                 (Tile c z)
            | otherwise = rotErr
  Edge (Tile a x) (Tile b y) -> edge
    where edge
            | face == a = Edge (Tile a x)
                               (Tile (rotFace b face dir) y)
            | face == b = Edge (Tile (rotFace a face dir) x)
                               (Tile b y)
            | otherwise = rotErr

rotate :: Direction -> Face -> Cube -> Cube
rotate dir face cube = Cube $
  getBlocksOff face cube ++
  (map (rotOneBlock dir face) $ getBlocksOn face cube)

{- a Rotation is a "move" that can be made in the standard notation -}

data Rotation = Rot Face
              | U' | L' | F' | R' | B' | D'
              | U2 | L2 | F2 | R2 | B2 | D2
              | Null | Break    -- "Break" for separating algorithms
              deriving (Eq)

instance Show Rotation where
  show r = case r of
    Rot U -> "\x1B[29mU\x1B[0m" -- white "U"
    Rot L -> "\x1B[32mL\x1B[0m" -- green "L"
    Rot F -> "\x1B[31mF\x1B[0m" -- red "F"
    Rot R -> "\x1B[34mR\x1B[0m" -- blue "R"
    Rot B -> "\x1B[36mB\x1B[0m" -- cyan "B"
    Rot D -> "\x1B[33mD\x1B[0m" -- yellow "D"
    U' -> "\x1B[29mU'\x1B[0m" -- white "U'"
    L' -> "\x1B[32mL'\x1B[0m" -- green "L'"
    F' -> "\x1B[31mF'\x1B[0m" -- red "F'"
    R' -> "\x1B[34mR'\x1B[0m" -- blue "R'"
    B' -> "\x1B[36mB'\x1B[0m" -- cyan "B'"
    D' -> "\x1B[33mD'\x1B[0m" -- yellow "D'"
    U2 -> "\x1B[29mU2\x1B[0m" -- white "U2"
    L2 -> "\x1B[32mL2\x1B[0m" -- green "L2"
    F2 -> "\x1B[31mF2\x1B[0m" -- red "F2"
    R2 -> "\x1B[34mR2\x1B[0m" -- blue "R2"
    B2 -> "\x1B[36mB2\x1B[0m" -- cyan "B2"
    D2 -> "\x1B[33mD2\x1B[0m" -- yellow "D2"
    Null -> "Null"
    Break -> "\n"

doubleRot :: Rotation -> Rotation
doubleRot r = case r of
  Rot U -> U2; Rot L -> L2; Rot F -> F2
  Rot R -> R2; Rot B -> B2; Rot D -> D2
  U' -> U2; L' -> L2; F' -> F2
  R' -> R2; B' -> B2; D' -> D2
  _  -> Null

oppositeRot :: Rotation -> Rotation
oppositeRot r = case r of
  Rot U -> U'; Rot L -> L'; Rot F -> F'
  Rot R -> R'; Rot B -> B'; Rot D -> D'
  U' -> Rot U; L' -> Rot L; F' -> Rot F
  R' -> Rot R; B' -> Rot B; D' -> Rot D
  r -> r

{- a solution is a list of rotations -}

data Solution = Solution {getSolution :: [Rotation]}

reduceRotations_, reduceRotations :: [Rotation] -> [Rotation]
reduceRotations_ [] = []
reduceRotations_ [r] = [r]
reduceRotations_ (Null:rs) = reduceRotations_ rs
reduceRotations_ (r0:r1:rs)
  | r0 == r1             = doubleRot r0 : reduceRotations_ rs
  | r0 == oppositeRot r1 = reduceRotations_ rs
  | r0 == doubleRot r1   = oppositeRot r1 : reduceRotations_ rs
  | doubleRot r0 == r1   = oppositeRot r0 : reduceRotations_ rs
  | otherwise            = r0 : reduceRotations_ (r1:rs)
reduceRotations rs =
  until (reduceRotations_ (reduceRotations_ rs) ==) reduceRotations_ rs

instance Show Solution where
  show = foldr ((++) . (" "++)) ""
         . map show
         . filter (/=Null)
         . reduceRotations
         . getSolution

{- State with cube and list of steps that have been done -}

type CubeState = State (Cube,Solution) ()

runCube :: CubeState -> Cube -> Cube
runCube action cube = fst $ execState action (cube, Solution [])

runSteps :: CubeState -> Cube -> Solution
runSteps action cube = snd $ execState action (cube, Solution [])

break :: CubeState
break = modify $ \(cube,(Solution rs)) -> (cube, Solution $ rs ++ [Break])

{- basic rotations on the cube -}

rot :: Direction -> Face -> Rotation -> CubeState
rot dir face r = modify $ \(cube, (Solution rs))
                          -> (rotate dir face cube, Solution $ rs ++ [r])

u, l, f, r, b, d :: CubeState
u = rot CW U (Rot U)
l = rot CW L (Rot L)
f = rot CW F (Rot F)
r = rot CW R (Rot R)
b = rot CW B (Rot B)
d = rot CW D (Rot D)

u', l', f', r', b', d' :: CubeState
u' = rot CCW U U'
l' = rot CCW L L'
f' = rot CCW F F'
r' = rot CCW R R'
b' = rot CCW B B'
d' = rot CCW D D'

-- u2, l2, f2, r2, b2, d2 :: CubeState
-- u2 = replicateM_ 2 u
-- l2 = replicateM_ 2 l
-- f2 = replicateM_ 2 f
-- r2 = replicateM_ 2 r
-- b2 = replicateM_ 2 b
-- d2 = replicateM_ 2 d

{- example Cube, list of all BlockP's, list of all Spots -}

allSpots :: [Spot]
allSpots = [(Corner B U L, U), -- W
            (Edge   B U,   U), -- W
            (Corner B U R, U), -- W

            (Edge   L U,   U), -- W
            (Edge   R U,   U), -- W

            (Corner F U L, U), -- W
            (Edge   F U,   U), -- W
            (Corner F U R, U), -- W

            (Corner L U B, L), -- G
            (Edge   L U,   L), -- G
            (Corner L U F, L), -- G
            (Corner F U L, F), -- R
            (Edge   F U,   F), -- R
            (Corner F U R, F), -- R
            (Corner R U F, R), -- B
            (Edge   R U,   R), -- B
            (Corner R U B, R), -- B
            (Corner B U R, B), -- O
            (Edge   B U,   B), -- O
            (Corner B U L, B), -- O

            (Edge   L B,   L), -- G
            (Edge   L F,   L), -- G
            (Edge   F L,   F), -- R
            (Edge   F R,   F), -- R
            (Edge   R F,   R), -- B
            (Edge   R B,   R), -- B
            (Edge   B R,   B), -- O
            (Edge   B L,   B), -- O

            (Corner L D B, L), -- G
            (Edge   L D,   L), -- G
            (Corner L D F, L), -- G
            (Corner F D L, F), -- R
            (Edge   F D,   F), -- R
            (Corner F D R, F), -- R
            (Corner R D F, R), -- B
            (Edge   R D,   R), -- B
            (Corner R D B, R), -- B
            (Corner B D R, B), -- O
            (Edge   B D,   B), -- O
            (Corner B D L, B), -- O

            (Corner D F L, D), -- Y
            (Edge   D F,   D), -- Y
            (Corner D F R, D), -- Y

            (Edge   D L,   D), -- Y
            (Edge   D R,   D), -- Y

            (Corner D B L, D), -- Y
            (Edge   D B,   D), -- Y
            (Corner D B R, D)  -- Y
            ]

allPositions :: [BlockP]
allPositions = [Corner F U L,
                Edge   F U,
                Corner F U R,
                Edge   F R,
                Corner F D R,
                Edge   F D,
                Corner F D L,
                Edge   F L,

                Edge L U,
                Edge L D,
                Edge R U,
                Edge R D,

                Corner B U L,
                Edge   B U,
                Corner B U R,
                Edge   B R,
                Corner B D R,
                Edge   B D,
                Corner B D L,
                Edge   B L
               ]

allBlocks :: [BlockF]
allBlocks = allPositions

solvedCube :: Cube
solvedCube = Cube [Corner (Tile F F) (Tile U U) (Tile L L), -- Front layer of blocks
                   Edge   (Tile F F) (Tile U U),            -- clockwise from top left.
                   Corner (Tile F F) (Tile U U) (Tile R R),
                   Edge   (Tile F F) (Tile R R),
                   Corner (Tile F F) (Tile D D) (Tile R R),
                   Edge   (Tile F F) (Tile D D),
                   Corner (Tile F F) (Tile D D) (Tile L L),
                   Edge   (Tile F F) (Tile L L),

                   Edge (Tile L L) (Tile U U),         -- Middle layer
                   Edge (Tile L L) (Tile D D),
                   Edge (Tile R R) (Tile U U),
                   Edge (Tile R R) (Tile D D),

                   Corner (Tile B B) (Tile U U) (Tile L L), -- Back layer of blocks
                   Edge   (Tile B B) (Tile U U),
                   Corner (Tile B B) (Tile U U) (Tile R R),
                   Edge   (Tile B B) (Tile R R),
                   Corner (Tile B B) (Tile D D) (Tile R R),
                   Edge   (Tile B B) (Tile D D),
                   Corner (Tile B B) (Tile D D) (Tile L L),
                   Edge   (Tile B B) (Tile L L)
                  ]
