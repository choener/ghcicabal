
-- | Calls @ghci@ with all extensions of all subprojects, and also provides all
-- such paths to @ghci. This makes it possible to develop multiple packages,
-- that are interdependent, in parallel.

module Main where

import Common



main = runMain "ghc -j -O3 -fllvm -optlo-O3 -rtsopts -threaded -ddump-to-file -ddump-simpl -dsuppress-all -outputdir .buildcabal" pWorkFile

