module Rymden.Content.Campaigns (campaigns, getLevel) where

import Prelude
import Rymden.Data.Campaign (Campaign)
import Rymden.Data.Level (Level)
import Data.Maybe (Maybe)
import Rymden.Data.Level (LevelId)
import Data.Array (concat)
import Data.Array (find)
import Rymden.Component.Levels.HelloRymden as HelloRymden

campaigns :: Array Campaign
campaigns =
  [ intro
  , boolean
  , natural
  ]

intro :: Campaign
intro =
  { id: "intro"
  , name: "Introduction"
  , levels:
      [ { id: "helloworld"
        , name: "What's rymden calculus?"
        }
      , { id: "variables"
        , name: "Variables"
        }
      ]
  }

boolean :: Campaign
boolean =
  { id: "boolean"
  , name: "Booleans"
  , levels:
      [ { id: "booleans"
        , name: "True or false"
        }
      ]
  }

natural :: Campaign
natural =
  { id: "natural"
  , name: "1, 2, 3"
  , levels: []
  }

getLevel :: LevelId -> Maybe Level
getLevel levelId =
  campaigns
    <#> _.levels
    # concat
    # find (\level -> level.id == levelId)
