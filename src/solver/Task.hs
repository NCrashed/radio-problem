module Task where

import Prelude as P
import Control.Concurrent.STM.TChan
import Data.Array.Repa as Repa
import Data.Functor
import Data.List as List
import qualified Data.Vector as Vec

-- | Field is two dimensional array of unboxed values of radio signal
newtype Field = Field (Array U DIM2 Int)

instance Show Field where
  show (Field f) = unlines . toList $ traverse f (\(Z:.h:._) -> Z:.h) $ 
    \getter (Z:.irow) -> concat $ show . getter <$> ix2 irow <$> [0 .. fw-1]
    where (Z:._:.fw) = extent f

-- | It's not necessary to carry whole field around
-- we can just pass field size to create a new field
-- for needed calculations 
type FieldSize = DIM2

-- | Creating of empty field
cleanField :: FieldSize -> Field
cleanField s = Field $ computeUnboxedS $ fromFunction s (const 0)

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

-- | Channel to pass plot points
type StatChannel = TChan (Int, Float)

newtype Chromosome = Chromosome (Vec.Vector Bool)
type Population = [Chromosome]

instance Show Chromosome where
  show (Chromosome chr) = concat . Vec.toList $ (\b -> if b then "1" else "0") <$> chr
  
-- | Returns only placed towers by solution in chromosome
filterTowers :: Chromosome -> [Tower] -> [Tower]
filterTowers (Chromosome chr) = P.map snd . filter ((== True) . fst) . zip (Vec.toList chr)

-- | Conversion helper
toFloat :: Int -> Float
toFloat = fromIntegral . toInteger

-- | Constructs network field from solution in chromosome
solutionField :: Task -> Chromosome -> Field
solutionField (Task fsize twrs radius) chr = foldl' placeTower field $ filterTowers chr twrs
  where 
    field = cleanField fsize
    placeTower :: Field -> Tower -> Field 
    placeTower (Field f) (tx, ty) = Field $ computeUnboxedS $ traverse f id $ 
      \getter sh -> getter sh + if inRadius sh then 1 else 0 
      where
        inRadius :: DIM2 -> Bool
        inRadius (Z :. y :. x) = (tx-x)*(tx-x) + (ty-y)*(ty-y) <= radius*radius

-- | Calculates percentage of network coverage by solution in chromosome
calcCoverage :: Task -> Chromosome -> Float
calcCoverage task = coverage . solutionField task
  where 
    coverage :: Field -> Float
    coverage (Field f) = toFloat covered / toFloat area
      where
        covered = foldl' (\i b -> if b>0 then i+1 else i) 0 (toList f)
        area = size $ extent f

-- | Calculates fitness for solution stored in chromosome
fitness :: Task -> Chromosome -> Float
fitness task@(Task _ twrs _) chr = coverage * minimal
  where coverage = calcCoverage task chr
        towersUsed = length $ filterTowers chr twrs
        towersCount = length twrs
        minimal = 1 - (toFloat towersUsed / toFloat towersCount)