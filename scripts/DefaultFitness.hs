module Fitness where

-- | Conversion helper
toFloat :: Int -> Float
toFloat = fromIntegral . toInteger

fitness :: Float -> Int -> Int -> Float
fitness coverage towersUsed towersCount = coverage * minimal
  where
    minimal = 1 - (toFloat towersUsed / toFloat towersCount) 