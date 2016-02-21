{-# LANGUAGE DeriveFunctor #-}
module Main where

import Data.IORef

import           Control.Monad.Free

import qualified Data.Map as Map
import           Data.Map (Map(..), (!))
import           Data.Monoid
import           Data.Maybe (fromJust)

type Bytes = String
type Path = String

data CloudFilesF a
  = SaveFile Path Bytes a
  | ListFiles ([Path] -> a)
  | GetFile Path (Bytes -> a)
  deriving (Functor)

type CloudFiles = Free CloudFilesF

saveFile :: Path -> Bytes -> CloudFiles ()
saveFile path payload = liftF $ SaveFile path payload ()

listFiles :: CloudFiles [Path]
listFiles = liftF $ ListFiles id

getFile :: Path -> CloudFiles Bytes
getFile path = liftF $ GetFile path id

simpleProgram :: CloudFiles Bytes
simpleProgram = do
  files <- listFiles
  if "myFile" `elem` files
    then getFile "myFile"
    else saveFile "myFile" "my bytes" >> getFile "myFile"

test :: IORef (Map Path Bytes) -> CloudFiles a -> IO a
test filesystem program = do
  case program of
    Pure last -> pure last
    Free (SaveFile p b next) -> do
      modifyIORef filesystem (Map.insert p b)
      test filesystem (next)
    Free (ListFiles next) -> do
      files <- Map.keys <$> readIORef filesystem
      test filesystem (next files)
    Free (GetFile p next) -> do
      contents <- fromJust . Map.lookup p <$> readIORef filesystem
      test filesystem (next contents)

main :: IO ()
main = do
  filesystem <- newIORef Map.empty
  programResults <- test filesystem simpleProgram
  putStrLn $ "Results: " <> programResults

