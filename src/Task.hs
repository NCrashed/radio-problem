module Task where

import Data.Array.Repa as Repa
import Data.Functor

-- | Field is two dimensional array of unboxed booleans
newtype Field = Field (Array U DIM2 Bool)

instance Show Field where
  show (Field f) = unlines . toList $ traverse f (\(Z:.h:._) -> Z:.h) $ 
    \getter (Z:.irow) -> concat $ toBit . getter <$> ix2 irow <$> [0 .. fw-1]
    where (Z:._:.fw) = extent f
          toBit b =  if b then "1" else "0"

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