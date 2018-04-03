module Main where

import Control.Monad
import Data.Complex
import Data.Int
import Graphics.UI.GLUT
import qualified Streamly as St
import qualified Streamly.Prelude as S

--import qualified Streamly.Prelude as S
--import qualified Control.Monad.Parallel as MP
iterations = 400

x // y = fromIntegral x / fromIntegral y

-- Divides [a] into [[a], [a], ...] with each sublist of length n,
-- except the last sublist which has length <= n.
chunkify n [] = []
chunkify n xs =
  let (xs', rest) = splitAt n xs
  in xs' : chunkify n rest

-- Converts a coordinate in screen space to a vertex.
pix2vert (Size w h) (x, y) =
  Vertex2 ((3 // w * fromIntegral x) - 2.0) ((2 // h * fromIntegral y) - 1.0)

-- List of all of the vertices that represent screen pixels.
vertices :: IO [Vertex2 GLfloat]
vertices =
  get windowSize >>= \(Size w h) ->
    return $ [pix2vert (Size w h) (x, y) | x <- [0 .. w - 1], y <- [0 .. h - 1]]

-- Gets the color for a number of iterations.
color3 r g b = Color3 r g b

getcolor :: Int -> Color3 Float
getcolor iter
  | iter == iterations = color3 0 0 0
  | otherwise = color3 (amt * 0.5) amt (amt * 0.5)
  where
    amt = iter // iterations

-- Returns the number of iterations <= the maximum iterations of the
-- Mandelbrot set at the given vertex.
mandel (Vertex2 r i) =
  length . takeWhile (\z -> magnitude z <= 2) . take iterations $
  iterate (\z -> z ^ 2 + (r :+ i)) 0

-- plots one point.
drawVert v = do
  color . getcolor $ mandel v
  vertex v

-- draws all the vertices in slices (to update the display while drawing).
display' :: [[Vertex2 GLfloat]] -> IO ()
display' chunks =
  let foo = S.each chunks :: St.ParallelT IO ([Vertex2 GLfloat]) -- O(n)
      bar =
        (\vs -> do
           renderPrimitive Points $ mapM_ drawVert vs
           flush -- ::[Vertex2 GLfloat] -> IO ()
         )
  in do S.mapM_ bar foo
        displayCallback $= display

-- draws the whole fractal
display :: IO ()
display = do
  clear [ColorBuffer]
  displayCallback $=
    (do v <- vertices
        let c = chunkify 256 v
        let (x:y:z:_) = chunkify 3 c
                                    --display' c
        St.runStreamT $
          St.liftIO (display' x) St.<|> St.liftIO (display' y) St.<|>
          St.liftIO (display' z))
  get currentWindow >>= postRedisplay

main = do
  getArgsAndInitialize
  initialDisplayMode $= [SingleBuffered, RGBMode]
  initialWindowSize $= Size 1200 1024
  initialWindowPosition $= Position 100 100
  createWindow "Mandelbrot"
  clearColor $= Color4 0 0 0 0
  matrixMode $= Projection
  loadIdentity
  ortho (-2) 1 (-1) 1 (-1) 1
  displayCallback $= display
  mainLoop
-- import Control.Parallel.Strategies
-- type Point = (Float, Float)
-- -- z = x + i y ; p = u + iv
-- -- next p z = z^2 + p -- order of application reversed for currying
-- next :: Point -> Point -> Point
-- next (x,y) (u,v) = (x * x - y * y + u, 2 * x * y + v)
-- mandelbrot :: Point -> [Point]
-- mandelbrot p = iterate (next p) (0,0)
-- julia :: Point -> Point -> [Point]
-- julia c = iterate (next c)
-- fairlyClose :: Point -> Bool
-- fairlyClose (u,v) = (u * u + v * v) < 100
-- inMandelbrotSet :: Point -> Bool
-- inMandelbrotSet p = all fairlyClose (mandelbrot p)
-- chooseColor :: [a] -> [Point] -> a
-- chooseColor palette = (palette !!) . length . take n . takeWhile fairlyClose
--                        where n = length palette - 1
-- type Image a = Point -> a
-- fracImage :: (Point -> [Point]) -> [a] -> Image a
-- fracImage fractal palette = chooseColor palette . fractal
-- type Grid a = [[a]]
-- grid :: Int -> Int -> Point -> Point -> Grid Point
-- grid c r (xmin, ymin) (xmax, ymax)
--   = [[(x,y) | x <- for c xmin xmax] | y <- for r ymin ymax]
-- for :: Int -> Float -> Float -> [Float]
-- for n min max = take n [min, min + delta ..]
--                 where delta = (max - min) / fromIntegral (n - 1)
-- pmap = parMap rseq
-- sample :: Grid Point -> Image a -> Grid a
-- sample points image = map (map image) points
-- draw ::
--   Grid Point
--   -> (Point -> [Point]) -- mandelbrot
--   -> [a] -- [Char]
--   -> (Grid a -> b) -- the charRender IO function
--   -> b
-- draw points fractal palette render
--   = render (sample points (fracImage fractal palette))
-- charPalette :: [Char]
-- charPalette = " ,.‘\"~:;o-!|?/<>X+={^O#%&@8*$"
-- --putStr . unlines
-- charRender :: Grid Char -> IO ()
-- charRender x = return ()--putStr . unlines
-- main :: IO ()
-- main = draw points mandelbrot charPalette charRender
--           where points = grid (1790 * 300) (1370 * 300) (-2.25,-1.5) (0.75,1.5)
-- figure2 = draw points(julia (0.32,0.043)) charPalette charRender
--        where points = grid 79 37 (-1.5,-1.5) (1.5,1.5)
