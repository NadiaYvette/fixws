#!/usr/bin/env -S stack
{- stack --resolver lts-22.28 --install-ghc --jobs 200 --compiler ghc-9.10.1 script --ghc-options -ignore-dot-ghci --package base --package containers --package transformers -}
-- vi: ft=haskell
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ViewPatterns          #-}

module Data.Char.FixWS
  ( processLine
  , processLineM) where

import qualified Control.Monad.Trans.RWS as RWS

import           Data.Bifunctor (bimap, first)
import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import           Data.Functor.Identity (Identity (..))
import           Data.Sequence (Seq, ViewL (..), ViewR (..), (|>))
import qualified Data.Sequence as Seq

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
  first Seq.viewl <$> RWS.get >>= \case
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

processLineM :: Monad m => String -> m String
processLineM s = Foldable.toList . snd <$>
  RWS.execRWST (mapM_ enqueueChar s) 8 (Seq.empty, 1)

processLine :: String -> String
processLine = runIdentity . processLineM

main :: IO () -- This could be mapM_ (putStrLn <=< processLineM)
main = mapM_ (putStrLn . processLine) . lines =<< getContents
