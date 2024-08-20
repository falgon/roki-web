module Utils.Base (
    mconcatM
) where

import           Control.Monad.Extra (mconcatMapM)

mconcatM :: (Monad m, Monoid b)
    => [m b]
    -> m b
mconcatM = mconcatMapM id

