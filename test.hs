module Test (test) where

import Cube

test = Cube [Corner (Tile F L) (Tile U B) (Tile L D),
             Edge   (Tile U F) (Tile F U),
             Corner (Tile F B) (Tile U D) (Tile R R),
             Edge   (Tile R D) (Tile F B),
             Corner (Tile F R) (Tile R D) (Tile D F),
             Edge   (Tile D U) (Tile F B),
             Corner (Tile F F) (Tile D U) (Tile L R),
             Edge   (Tile L R) (Tile F U),

             Edge   (Tile U R) (Tile L D),
             Edge   (Tile U D) (Tile R F),
             Edge   (Tile D B) (Tile R L),
             Edge   (Tile L R) (Tile D B),

             Corner (Tile U L) (Tile L B) (Tile B U),
             Edge   (Tile B L) (Tile U D),
             Corner (Tile U R) (Tile R B) (Tile B U),
             Edge   (Tile R F) (Tile B L),
             Corner (Tile B F) (Tile R U) (Tile D L),
             Edge   (Tile B R) (Tile D F),
             Corner (Tile B D) (Tile L L) (Tile D F),
             Edge   (Tile B U) (Tile L L)
             ]