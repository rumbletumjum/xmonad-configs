-- |

module Theme where

import XMonad (Dimension)

data Theme =
  Theme { borderNormalColor :: String
        , borderFocusedColor :: String
        , borderWidth :: Dimension
        } deriving Show

solarizedDark :: Theme
solarizedDark =
  Theme { borderNormalColor = "#002b36"
        , borderFocusedColor = "#d33682"
        , borderWidth = 2
        }
