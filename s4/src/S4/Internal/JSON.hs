module S4.Internal.JSON
 ( jsonOptions
 ) where

import Data.Aeson
  ( defaultOptions
  , camelTo2
  , fieldLabelModifier
  )

-- Aeson encoding options that turns camelCase into hyphenated-words.
jsonOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '-'
  }
