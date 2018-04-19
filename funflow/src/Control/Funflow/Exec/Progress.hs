-- | Progress reports from a running executor.
module Control.Funflow.Exec.Progress
  ( GraphUpdate
  , Progress(..)
  ) where

import qualified Data.Text as T

type NodeId = ()

data Status
  = Started
  | Cached
  | Finished
  | Errored
  deriving Show

data NodeUpdate
  = AddMetadata (T.Text, T.Text)
  | UpdateStatus Status
  deriving Show

data GraphUpdate
    -- | Introduce a node, with its parent nodes
  = IntroduceNode NodeId [NodeId]
  | UpdateNode NodeId NodeUpdate
  deriving Show

data Progress
  = Msg T.Text
  | GraphUpdate
  deriving Show
