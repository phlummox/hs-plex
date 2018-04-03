
{- |

Execute a command, redirect stderr into stdout, and return the combined
result (optionally, with a timeout). 

-}

module System.Plex
  ( 
    readCommand
    , cmd
    , cmdTimeout
  ) where

import System.Plex.Internal as I (
    readCommand
  , cmd
  , cmdTimeout
  )

