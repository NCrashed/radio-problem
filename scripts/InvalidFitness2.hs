module Fitness where

-- | Conversion helper
toFloat :: Int -> Float
toFloat = fromIntegral . toInteger

fitness :: Float -> Int -> Int -> Float
fitness coverage towersUsed _ = if k == 0 then 0 else coverage / k
  where
    k = toFloat towersUsed