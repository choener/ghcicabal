
-- | Calls @ghci@ with all extensions of all subprojects, and also provides all
-- such paths to @ghci. This makes it possible to develop multiple packages,
-- that are interdependent, in parallel.

module Main where

import Common



main = runMain "ghci"

