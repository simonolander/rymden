module Rymden.Data.Progress where

import Prelude
import Rymden.Data.Level (LevelId)
import Data.Array (any, (:))

type CompletedLevel
  = { levelId :: LevelId
    }

type Progress
  = { completedLevels :: Array CompletedLevel
    }

initialProgress :: Progress
initialProgress = { completedLevels: [] }

isLevelCompleted :: LevelId -> Progress -> Boolean
isLevelCompleted levelId progress = any (\completedLevel -> completedLevel.levelId == levelId) progress.completedLevels

withCompleteLevel :: LevelId -> Progress -> Progress
withCompleteLevel levelId progress =
  if isLevelCompleted levelId progress then
    progress
  else
    progress { completedLevels = { levelId } : progress.completedLevels }
