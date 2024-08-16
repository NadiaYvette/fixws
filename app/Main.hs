module Main where

import Prelude

import qualified Data.Char.FixWS as FixWS (fixws)

main :: IO ()
main = do
  input <- getContents
  mapM_ (putStrLn . FixWS.fixws) $ lines input
