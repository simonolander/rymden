module Rymden.Data.Campaign where

import Prelude
import Rymden.Data.Level (Level)

type CampaignId
  = String

type Campaign
  = { id :: CampaignId
    , name :: String
    , levels :: Array Level
    }
