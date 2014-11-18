module Graphics where

import Data.Array.Repa hiding ((++))
import Data.Functor
import Graphics.Gloss
import Task
import Genetic

applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ v = v
applyN n f v = applyN (n-1) f (f v)

drawSolution :: Task -> Chromosome -> IO ()
drawSolution task chr = display mode white $ solutionPicture task chr
  where mode = InWindow "Radio-problem solver" (1280, 1024) (10, 10)
  
solutionPicture :: Task -> Chromosome -> Picture
solutionPicture task@(Task _ twrs radius) chr = pictures $ cells ++ xlabels ++ ylabels ++ towers ++ circles ++ info
  where Field field = solutionField task chr
        (Z:.fw:._) = extent field
        cwidth = 200 :: Float
        cheight = cwidth :: Float
        
        cell 0 = rectangleWire cwidth cheight
        cell v = pictures [color (applyN v dark green) 
          $ rectangleSolid cwidth cheight, rectangleWire cwidth cheight]
        place (Z:.y:.x) = translate (cwidth * fromIntegral x) (- cheight * fromIntegral y)
        cells = toList $ traverse field id $ \getter sh -> place sh $ cell $ getter sh
        
        xtext i = translate (cwidth * (-0.2 + fromIntegral i)) (0.7*cheight) $ text (show i)
        ytext i = translate (- 1.5*cwidth) (- cheight * (0.3 + fromIntegral i)) $ text (show i)
        xlabels = let (Z:._:.w) = extent field in xtext <$> [0 .. w-1]
        ylabels = let (Z:.h:._) = extent field in ytext <$> [0 .. h-1]
        
        towers = (\t -> placeTower (t `elem` placedTowers) t) <$> twrs
        placedTowers = filterTowers chr twrs
        placeTower b (x,y) = translate (cwidth * fromIntegral x) (-cheight * (0.4 + fromIntegral y)) $ 
          tower $ greyN $ if b then 0.3 else 0.8
          
        circles = placeCircle <$> placedTowers
        placeCircle (x,y) = translate (cwidth * fromIntegral x) (-cheight * fromIntegral y) 
          $ circle (fromIntegral radius * cwidth)
          
        info = translate (cwidth*(fromIntegral fw + 1)) 0 <$> rows 150 ([
            text $ "Solution: " ++ show chr
          , text $ "Fitness: " ++ show (fitness task chr)  
          , text "Towers: "
          ] ++ (text . show <$> placedTowers))
        rows h txts = (\(dy, p) -> translate 0 (-dy) p) <$> zip ((h*) <$> [1 .. ]) txts
               
tower :: Color -> Picture
tower basecol = pictures $ scale 80 60 . color basecol <$> [triangle, dot, rwaves, lwaves]
  where triangle = polygon [(-0.5, 0), (0.5, 0), (0, 1.8)]
        dot = translate 0 2 $ circleSolid 0.2
        waves a = pictures $ translate 0 2 <$> rotate a <$> [thickArc 0 120 0.4 0.1, thickArc 0 120 0.6 0.1]
        rwaves = waves 60
        lwaves = waves (-120)