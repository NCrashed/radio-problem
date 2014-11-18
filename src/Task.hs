module Task where

import Data.Array.Repa

-- | Field is two dimensional array of unboxed booleans
newtype Field = Field (Array U DIM2 Bool)
-- | It's not necessary to carry whole field around
-- we can just pass field size to create a new field
-- for needed calculations 
type FieldSize = DIM2

-- | Creating of empty field
cleanField :: FieldSize -> Field
cleanField s = Field $ computeUnboxedS $ fromFunction s (const False)

-- | Tower is a point in Field
type Tower = (Int, Int)
-- | Radius of network generated with single tower
type Radius = Int

-- | Input data
data Task = Task FieldSize [Tower] Radius

-- | Options of evolution process
data EvolOptions = EvolOptions {
    mutationChance :: Float
  , elitePart :: Float
  , maxGeneration :: Int
  , popCount :: Int
  , indCount :: Int
  }