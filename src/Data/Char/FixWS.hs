#!/usr/bin/env -S stack
{- stack --resolver lts-22.28 --install-ghc --jobs 200 --compiler ghc-9.10.1 script --package base --package composition-extra --package containers --package transformers --package tuple --ghc-options -ignore-dot-ghci -}
-- vi: ft=haskell

-- Code formatters for the moment don't recognize HLINT or OPTIONS_GHC
-- pragmas or other ghc pragmas like COMPLETE or MINIMAL and it would be
-- nice if they did.
{-  ORMOLU_DISABLE                              -}
{-# LANGUAGE BlockArguments                    #-}
{-# LANGUAGE ExplicitNamespaces                #-}
{-# LANGUAGE LambdaCase                        #-}
{-# LANGUAGE PackageImports                    #-}
{-# LANGUAGE TupleSections                     #-}
{-# LANGUAGE ViewPatterns                      #-}
{-# HLINT ignore "Avoid restricted flags"      #-}
{-# HLINT ignore "Avoid restricted extensions" #-}
{-# OPTIONS_GHC -Wno-unused-top-binds          #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas      #-}
{-  ORMOLU_ENABLE                               -}

module Data.Char.FixWS
  ( fixWSLineM
  , fixWSLine
  , fixWSFile
  , fixWSstdio) where

-- stylish-haskell aligns the PackageImports but has a bug with its
-- import list where the padding is supposed to be relative to alignment
-- beneath the module, but is actually interpreted relative to the
-- beginning of the line. None have the export list method I'd like.
{-  ORMOLU_DISABLE                              -}
import                               Control.Arrow ((***))
import qualified                     Control.Arrow as Arrow (first)
import qualified "transformers"      Control.Monad.Trans.RWS as RWS
                                       ( RWST (..), ask, execRWST, get
                                       , modify, put, tell)

import qualified                     Data.Char as Char (isSpace)
import qualified                     Data.Foldable as Foldable (toList)
import           "composition-extra" Data.Function.Contravariant.Syntax ((-.))
import           "composition-extra" Data.Functor.Syntax ((<$$>))
import                               Data.Functor.Identity (Identity (..))
import           "containers"        Data.Sequence
                                       ((|>), Seq , ViewL (..), ViewR (..))
import qualified "containers"        Data.Sequence as Sequence
                                       ( empty, replicate, singleton
                                       , viewl, viewr)
import qualified                     Data.String as String (lines, unlines)
import qualified "tuple"             Data.Tuple.All as Tuple (uncurryN)

import qualified                     System.IO as IO
                                       (Handle, hGetContents, putStr, stdin)

import                               Prelude
                                       ( (>>), (=<<), (>>=), (==), ($)
                                       , (<$>), (.), (+), (-), (*)
                                       , type Applicative (..), type Char
                                       , type Int, type Integral (..)
                                       , type IO, type Monad (..)
                                       , type Num (..), type String
                                       , otherwise)
import qualified Prelude (const, mapM_, snd)
{-  ORMOLU_ENABLE                               -}

type WSMonad monad t = RWS.RWST Int (Seq Char) (Seq (Char, Int), Int) monad t

-- Deliberately drop all pending whitespace upon encountering newline.
enqueueChar :: Monad monad => Char -> WSMonad monad ()
enqueueChar '\LF' = do
  RWS.tell $ Sequence.singleton '\LF'
  RWS.put   (Sequence.empty, 1)
enqueueChar c
  | Char.isSpace c = RWS.modify $ Arrow.first \case
      q@(Sequence.viewr -> (rest :> (c', n)))
          | c == c'   -> rest |> (c, n + 1)
          | otherwise ->    q |> (c, 1)
      _ {- EmptyL Sequence -} -> Sequence.singleton (c, 1)
  -- Upon encountering non-whitespace, flush pending queue to the writer.
  | otherwise = do flushQueue
                   RWS.tell $ Sequence.singleton c
                   RWS.modify $ Prelude.const Sequence.empty *** (+ 1)

flushQueue :: Monad monad => WSMonad monad ()
flushQueue = RWS.ask >>= \width -> do
  RWS.get >>= Arrow.first Sequence.viewl -. \case
    (EmptyL, _) -> pure ()
    ((c, n) :< rest, column)
      | '\HT' <- c
      , m <- width - (column `rem` width) + 1 + (n - 1) * width
      -> mv (rest, '\SP', m, column + m)
      | '\LF' <- c -> mv (rest, c, n, 1)
      | otherwise  -> mv (rest, c, n, column + n)
  where mv (q, c', k, p) = do RWS.tell (Sequence.replicate k c')
                              RWS.put (q, p)
                              flushQueue

fixWSLineM :: Monad monad => String -> monad String
fixWSLineM
  =    Foldable.toList
  .    Prelude.snd
  <$$> Tuple.uncurryN RWS.execRWST
  .    (, 8, (Sequence.empty, 1))
  .    Prelude.mapM_ enqueueChar

fixWSLine :: String -> String
fixWSLine = runIdentity . fixWSLineM

-- The stock rules for hlint don't account for composition-extra's
-- operators' fixities and precedences. Therefore it's better to disable
-- the warning than to uglify the code.
{-# HLINT ignore fixWSFile "Functor law" #-}
fixWSFile :: IO.Handle -> IO String
fixWSFile fileHandle =
  String.unlines
    <$> fixWSLine
      <$$> String.lines
    <$> IO.hGetContents fileHandle

-- This could be mapM_ (putStrLn <=< processLineM)
fixWSstdio :: IO ()
fixWSstdio = IO.putStr =<< fixWSFile IO.stdin

main :: IO ()
main = fixWSstdio
