module Main (main) where

import Parse (test) -- Explicitly import 'test'
import Data.Text (pack)

main :: IO ()
main = do
    content <- readFile "test/input/HelloWorld.scm" -- Read file as String
    let input = pack content                        -- Convert String to Text
    test input        