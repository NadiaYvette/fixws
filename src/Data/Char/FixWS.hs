module Data.Char.FixWS
  ( fixws ) where

import           Prelude

import           Control.Conditional ((?), (??))
-- import qualified Control.Conditional as Cond
-- import qualified Control.Monad as Monad (when)
import Control.Monad.Trans.RWS (RWST)
import qualified Control.Monad.Trans.RWS as RWS
                   ( ask
                   , asks
                   , evalRWST
                   , get
                   , put
                   , modify
                   , tell)

import qualified Data.Char as Char
                   ( isPrint
                   , isSpace)

import           Data.Functor.Identity
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import           GHC.Exts
import           GHC.IsList

fixws :: ScreenConstraints Identity string => string -> string
fixws s = snd . runIdentity $ RWS.evalRWST
  (mapM_ emit $ toList s :: ScreenConstraints Identity string => ScreenMonad Identity string ())
  ScreenReader
  { srTabWidth = 2 }
  ScreenState
  { ssCurrentColumn = 1
  , ssLastPrintable = Nothing
  , ssPendingSpaces = 0
  , ssTabStopList = [] }

type Column = Int
type ScreenConstraints monad string =
  ( Monad monad
  , IsString string
  , IsList string
  , Item string ~ Char
  , Monoid string)


-- | `ScreenReader` is a tabstop width for when the list of hand-set
--   tabstops runs out. The list of tabstops would, in a way, be good
--   to have here; however, the algorithm now in use destructively
--   updates the state while traversing it in parallel with the string
--   representing a line.
data ScreenReader =
  ScreenReader
  { srTabWidth :: Column }

type ScreenWriter string = string

-- | `ScreenState` is a pair of integers representing the next
--   tabstop and the cursor's position while emitting.
data ScreenState = -- (Int, [Int])
  ScreenState
  { ssCurrentColumn :: Column
  , ssLastPrintable :: Maybe Column
  , ssPendingSpaces :: Column
  , ssTabStopList :: [Column] }

-- | The `ScreenMonad` type is the relevant type of a monadic action
--   carried out while emitting characters.
type ScreenMonad m string t =
  RWST ScreenReader (ScreenWriter string) ScreenState m t

-- | `emit` is supposed to only emit one character, but the character
--   types associated with a string structure aren't easily associable.
emit :: forall monad string . ()
  => ScreenConstraints monad string
  => Char
  -> ScreenMonad monad string ()
emit c = do
  ScreenReader { .. } <- RWS.ask
  ScreenState { .. } <- RWS.get
  case c of
    '\HT'
      | nextTabStop : tabStop' <- ssTabStopList
      , advance <- nextTabStop - ssCurrentColumn
      -> RWS.modify \state -> state
          { ssCurrentColumn = nextTabStop
          , ssPendingSpaces = ssPendingSpaces + advance
          , ssTabStopList = tabStop' }
      | columnRemainder <- ssCurrentColumn `rem` srTabWidth
      , advance <- srTabWidth - columnRemainder
      -> RWS.modify \state -> state
          { ssCurrentColumn = ssCurrentColumn + advance
          , ssPendingSpaces = ssPendingSpaces + advance }
    _ | newColumn <- ssCurrentColumn + 1
      , (prefix, newPendingSpaces) <- Char.isPrint c
          ?  (replicate ssPendingSpaces '\SP', 0)
          ?? ([], ssPendingSpaces + 1)
      , newTabStops <- Maybe.fromJust . (List.uncons ssTabStopList >>=) $
          \(nextTabStop, tabStops') -> Just $
              newColumn >= nextTabStop ? tabStops' ?? ssTabStopList
      -> do RWS.tell . fromString $ prefix <> [c]
            RWS.modify \state -> state
              { ssCurrentColumn = newColumn
              , ssTabStopList = newTabStops }
      {-
      , newTabStops <- flip (maybe []) (List.uncons ssTabStopList)
                           \case (nextTabStop, tabStops')
                                 | newColumn >= nextTabStop -> tabStops'
                                 | otherwise -> ssTabStopList
                                 -}
                           -- \(nextTabStop, tabStops') -> newColumn >= nextTabStop ? tabStops' ?? ssTabStopList
