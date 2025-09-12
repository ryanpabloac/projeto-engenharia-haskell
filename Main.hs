module Main (main) where

import Interface (hubMenus)
import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))

main :: IO ()
main = do 
    hSetBuffering stdout NoBuffering
    putStrLn "=== MENU ==="
    hubMenus "principal"
