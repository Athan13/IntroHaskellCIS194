module Main where

import StringBuffer
import Editor
import JointList
import Sized
import Scrabble

-- main = runEditor editor $ unlines
--          [ "This buffer is for notes you don't want to save, and for"
--          , "evaluation of steam valve coefficients."
--          , "To load a different file, type the character L followed"
--          , "by the name of the file."
--          ]

main = runEditor editor (Single (Score 45, Size 1) "This is a new temporary buffer!")
