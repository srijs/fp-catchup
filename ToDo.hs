module Main where

import Data.List

import System.Console.Haskeline

data Item = Item String
  deriving (Eq, Show, Read)

instance Ord Item where
  compare (Item _) (Item _) = undefined

data Command = Insert String
             | Delete String
             | Quit
  deriving (Read)

takeCommand :: InputT IO Command
takeCommand = do
  line <- getInputLine "Awaiting your command: "
  case line of
    Nothing -> return Quit
    Just cmd -> return (read cmd)

outputItems :: [Item] -> InputT IO ()
outputItems [] = outputStrLn "-- end of list --"
outputItems (Item title : items) = do
  outputStrLn (show title)
  outputItems items

loop :: [Item] -> InputT IO ()
loop items = do
  outputStrLn "Your ToDo List:"
  outputItems items
  cmd <- takeCommand
  case cmd of
    Insert text -> loop (items ++ [Item text])
    Delete text -> loop (delete (Item text) items)
    Quit -> return ()

-- Tasks:
-- 1. Implement priorities for items
-- 2. Implement a comamnd "Maximum", that gives you the highest-prio item
-- 3. Implement a command "Move String Int Int", that moves an item from a position to another

main :: IO ()
main = runInputT (defaultSettings {
  historyFile = Just "./hist"
  }) (loop [])
