module Main (main) where

import Interface (hubMenus)

main :: IO ()
main = do
    putStrLn "=== MENU ==="
    hubMenus "principal"
