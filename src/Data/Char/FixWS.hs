#!/usr/bin/env -S stack
{- stack --resolver lts-22.28 --install-ghc --jobs 200 --compiler ghc-9.10.1 script --package base --package composition-extra --package containers --package transformers --ghc-options -ignore-dot-ghci -}
-- vi: ft=haskell
{-# LANGUAGE BlockArguments           #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE PackageImports           #-}
{-# LANGUAGE ViewPatterns             #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Data.Char.FixWS
  ( fixWSLineM
  , fixWSLine
  , fixWSFile
  , fixWSstdio) where

import qualified "transformers" Control.Monad.Trans.RWS as RWS

import           Data.Bifunctor (bimap, first)
import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import           "composition-extra" Data.Function.Contravariant.Syntax ((-.))
import           "composition-extra" Data.Functor.Syntax ((<&&>))
import           Data.Functor ((<&>))
import           Data.Functor.Identity (Identity (..))
import           "containers" Data.Sequence (Seq, ViewL (..), ViewR (..), (|>))
import qualified "containers" Data.Sequence as Seq
import qualified System.IO as IO
import           Prelude

type WSMonad m t = RWS.RWST Int (Seq Char) (Seq (Char, Int), Int) m t

enqueueChar :: Monad m => Char -> WSMonad m ()
-- Deliberately drop all pending whitespace upon encountering newline.
enqueueChar '\LF' = RWS.tell (Seq.singleton '\LF') >> RWS.put (Seq.empty, 1)
enqueueChar c
  | Char.isSpace c = RWS.modify $ first \case
        q@(Seq.viewr -> (rest :> (c', n))) | c == c'   -> rest |> (c, n + 1)
                                           | otherwise ->    q |> (c, 1)
        _ {- EmptyL Sequence -} -> Seq.singleton (c, 1)
  -- Upon encountering non-whitespace, flush pending queue to the writer.
  | otherwise = do flushQueue
                   RWS.tell $ Seq.singleton c
                   RWS.modify $ const Seq.empty `bimap` (+ 1)

flushQueue :: Monad m => WSMonad m ()
flushQueue = RWS.ask >>= \width -> do
  RWS.get >>= first Seq.viewl -. \case
    (EmptyL, _) -> pure ()
    ((c, n) :< rest, column)
      | '\HT' <- c
      , m <- width - (column `rem` width) + 1 + (n - 1) * width
      -> mv (rest, '\SP', m, column + m)
      | '\LF' <- c -> mv (rest, c, n, 1)
      | otherwise  -> mv (rest, c, n, column + n)
  where mv (q, c', k, p) = do RWS.tell (Seq.replicate k c')
                              RWS.put (q, p)
                              flushQueue

fixWSLineM :: Monad m => String -> m String
fixWSLineM s = Foldable.toList . snd <$>
  RWS.execRWST (mapM_ enqueueChar s) 8 (Seq.empty, 1)

fixWSLine :: String -> String
fixWSLine = runIdentity . fixWSLineM

fixWSFile :: IO.Handle -> IO String
fixWSFile fileHandle = IO.hGetContents fileHandle <&> lines <&&> fixWSLine <&> unlines

-- This could be mapM_ (putStrLn <=< processLineM)
fixWSstdio :: IO ()
fixWSstdio = putStr =<< fixWSFile IO.stdin

main :: IO ()
main = fixWSstdio
