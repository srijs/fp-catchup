module Main where

import Data.List

import System.Console.Haskeline

data Item = Item Int String
  deriving (Eq, Show, Read)

instance Ord Item where
  compare (Item a _) (Item b _) = compare a b

data Command = Insert String Int
             | Delete String Int
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
outputItems (Item prio title : items) = do
  outputStrLn (show prio ++ ": " ++ title)
  outputItems items

loop :: [Item] -> InputT IO ()
loop items = do
  outputStrLn "Your ToDo List:"
  outputItems items
  cmd <- takeCommand
  case cmd of
    Insert text prio -> loop (insert (Item prio text) items)
    Delete text prio -> loop (delete (Item prio text) items)
    Quit -> return ()

main :: IO ()
main = runInputT defaultSettings (loop [])
