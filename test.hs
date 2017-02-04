module Test (test) where

import Cube

test = Cube [Corner (Tile F U) (Tile U B) (Tile L L),
             Edge (Tile U B) (Tile F R),
             Corner (Tile F R) (Tile U U) (Tile R B),
             Edge (Tile R F) (Tile F U),
             Corner (Tile F D) (Tile R B) (Tile D L),
             Edge (Tile D B) (Tile F D),
             Corner (Tile F F) (Tile D D) (Tile L L),
             Edge (Tile L D) (Tile F R),

             Edge (Tile U U) (Tile L L),
             Edge (Tile U D) (Tile R L),
             Edge (Tile D U) (Tile R R),
             Edge (Tile L B) (Tile D L),

             Corner (Tile U F) (Tile L R) (Tile B D),
             Edge (Tile B B) (Tile U U),
             Corner (Tile U F) (Tile R R) (Tile B U),
             Edge (Tile R F) (Tile B R),
             Corner (Tile B U) (Tile R L) (Tile D F),
             Edge (Tile B D) (Tile D F),
             Corner (Tile B B) (Tile L D) (Tile D R),
             Edge (Tile B L) (Tile L F)
             ]