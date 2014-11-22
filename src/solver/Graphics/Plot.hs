module Graphics.Plot where

import Graphics.Gloss
import Data.Monoid
import Data.List
import Data.Function
import Data.Functor
import Text.Printf

takeUniform :: Int -> [a] -> [a]
takeUniform n l
  | n > length l = error "n is larger than passed list!"
  | otherwise = take n $ every step l 
  where
    step = round $ (fromIntegral (length l) :: Float) / fromIntegral n
    every k xs = case drop (k-1) xs of
              (y:ys) -> y : every k ys
              [] -> []
              
plot :: String -> String -> [Point] -> Picture
plot xstr ystr pts = coords <> xlabel <> ylabel <> grid <> plotted
  where coords = xcoord <> ycoord
        xcoord = line [(0, 0), (1,0)] <> translate 1 0 xarrow
        ycoord = line [(0, 0), (0,1)] <> translate 0 1 yarrow
        xarrow = line [(-0.05, 0.01), (0, 0), (-0.05, -0.01)]
        yarrow = rotate (-90) xarrow
        
        xmin = fst $ minimumBy (compare `on` fst) pts
        xmax = fst $ maximumBy (compare `on` fst) pts
        ymin = snd $ minimumBy (compare `on` snd) pts
        ymax = snd $ maximumBy (compare `on` snd) pts
        
        ymargin = 0.1
        xmargin = 0.1
        xrange = let v = xmax - xmin in if v == 0 then 1.0 else v
        yrange = let v = ymax - ymin in if v == 0 then 1.0 else v
        toLocal (x, y) = ( xmargin + (1 - 2*xmargin) * (x - xmin) / xrange
                         , ymargin + (1 - 2*ymargin) * (y - ymin) / yrange)
        fromLocalX x = xmin + (x - xmargin) * xrange / (1 - 2*xmargin)
        fromLocalY y = ymin + (y - ymargin) * yrange / (1 - 2*ymargin)
                           
        localPts = toLocal <$> pts
        intervals 
          | null localPts = [] 
          | length localPts == 1 = [(head localPts, head localPts)] 
          | otherwise = localPts `zip` tail localPts
        plotted = color red $ mconcat $ (\(s, e) -> line [s, e]) <$> intervals
        
        ltexscale = 0.0006
        xlabel = translate 0.8 (-0.1) $ scale ltexscale ltexscale $ text xstr
        ylabel = translate 0.05 1 $ scale ltexscale ltexscale $ text ystr
        
        gridPts 
          | length localPts <= 2 = localPts
          | otherwise = head localPts : (middle ++ [last localPts])
          where
            middlePts = tail $ init localPts
            middle = takeUniform (min 10 (length middlePts)) middlePts
            
        smalltext :: Int -> Float -> Picture
        smalltext n = scale 0.0003 0.0003 . text . printf ("%."++show n++"f") 
        xgrid = mconcat $ (\x -> line [(x,0),(x,1)] <> translate (x-0.025) (-0.05) (smalltext 0 $ fromLocalX x)) . fst <$> gridPts
        ygrid = mconcat $ (\y -> line [(0,y),(1,y)] <> translate (-0.1) (y-0.012) (smalltext 2 $ fromLocalY y)) . snd <$> gridPts
        grid = color (greyN 0.5) $ xgrid <> ygrid

sample :: Float -> Float -> Int -> (Float -> Float) -> [(Float, Float)]
sample xmin xmax i f = (\x -> (x, f x)) <$> ((\j -> xmin + (xmax - xmin) * fromIntegral j / fromIntegral i) <$> [0 .. i - 1]) 