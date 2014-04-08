{-# LANGUAGE QuasiQuotes #-}

import Prelude hiding (map)
import System.Environment
import Data.Array.Repa
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2
import Data.Array.Repa.IO.Matrix

_nx      = 50                      :: Int
_ny      = 20                      :: Int
_ip      = 40                      :: Int
_jp      = 14                      :: Int
_l      = 500                      :: Double
_dx      = _l / fromIntegral _nx   :: Double
_w      = 100                      :: Double
_dy      = _w / fromIntegral _ny   :: Double
_uo      = 1.0                     :: Double

u :: Array U DIM2 Double
u = fromListUnboxed (Z :. _nx + 1 :. _ny + 1) list
  where
    list  = concat $ first:rest
    -- The first row of the matrix is [0..100]
    first = [_uo * fromIntegral (j - 1) * _dy | j <- [0.._ny-1]]
            Prelude.++ [_w] -- Last element is _w but we can't generate it.
    rest  = replicate _nx $ replicate _ny 0 Prelude.++ [_uo * _w]

boundMask :: Monad m => m (Array U DIM2 Double)
boundMask = computeP
            -- Convert the boolean into a Double
            $ map (fromIntegral . fromEnum)
            -- Generate the boundary mask of size (_nx+1, _ny+1)
            -- from a boolean function of the indeces.
            $ fromFunction (Z :. _nx + 1 :. _ny + 1) f
  where f ix =
          -- The very edges count as boundaries for the stencil.
          inShapeRange (Z :. 1  :.  1) (Z :. _nx  :.  _ny) ix
          -- Draw a bounding box from (_ip, 0) to (_nx+1, 0)
          && not (inShapeRange (Z :. _ip :. 0) (Z :. _nx+1 :.  _jp) ix)

laplaceStencil :: Array U DIM2 Double -> Array (TR PC5) DIM2 Double
laplaceStencil = mapStencil2 (BoundConst 0)
                 [stencil2| 0 1 0
                            1 0 1
                            0 1 0 |]

-- Based on the Laplace solver from repa-examples; see
-- http://hackage.haskell.org/package/repa-examples
solve :: Monad m =>
         Int ->                                -- Number of iterations
         Array U DIM2 Double ->                -- Boundary mask
         Array U DIM2 Double ->                -- Bounds of the array
         Array U DIM2 Double ->                -- The array to iterate
         m (Array U DIM2 Double)
solve 0 _    _      a = return a
solve n mask bounds a = computeP
                        (bounds +^             -- Add the bounds.
                         (mask *^ smap (/ 4.0)  -- Multiply by the stencil,
                                               -- then divide by 4.
                          (laplaceStencil a)))
                        >>= \a' -> solve (n - 1) mask bounds a'

-- Run the simulation.
simulate :: Monad m => Int -> m (Array U DIM2 Double)
simulate nIter = boundMask >>= \mask -> solve nIter mask u u

main :: IO ()
main = do
  args <- getArgs
  case args of [outPath, n] ->
                 (simulate . read) n
                 >>= writeMatrixToTextFile outPath
               _ -> print "usage: ./fluid output.txt 100"
