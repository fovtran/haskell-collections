import Control.Monad
import Data.IORef
import qualified Data.Map as M
import AllHailGeometricAlgebra
import Haste
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas
import Haste.Graphics.AnimationFrame

vs = replicateM 3 [-1, 1]

fs :: [[Int]]
fs = [[0, 1, 3, 2], [4, 6, 7, 5],
      [0, 4, 5, 1], [2, 3, 7, 6],
      [1, 5, 7, 3], [0, 2, 6, 4]]

-- Darken, so the white side is visible.
cs = map (\c -> c * 220 `div` 255) <$>
-- http://the-rubiks-cube.deviantart.com/journal/Using-Official-Rubik-s-Cube-Colors-268760351
  [[0, 0x51, 0xba], [0, 0x9e, 0x60],
   [0xff, 0xd5, 0], [0xff, 0xff, 0xff],
   [0xff, 0x58, 0], [0xc4, 0x1e, 0x3a]]

rotInv :: Multivector -> Multivector
rotInv = M.mapWithKey $ \k v -> if null k then v else -v

fromGA :: Multivector -> [Double]
fromGA m = (m!) . pure  <$> "xyz"

toGA :: [Double] -> Multivector
toGA = M.fromList . zip (pure <$> "xyz")

rx t = rotr [1, 0, 0] $ t * pi / 2
rz t = rotr [0, 0, 1] $ t * pi / 2
mv1 = M.fromList [("", 1)]

vshadeQ tRaw = tra . rot where
  rot = fromGA . conj r . toGA
  r | even sec = rbase
    | odd  sec = gMul rbase $ ([rx, rz]!!(div sec 2 `mod` 2)) $ t / 1000
  tra [x, y, z] = [x + 2, y + 2, z + 5]
  (sec, msec) = (tRaw `mod` 12000) `divMod` 1000
  rs = scanl gMul mv1 $ cycle [rx 1, rz 1]
  rbase = rs!!div sec 2
  t = fromIntegral msec

r111 t = rotr [1, 1, 1] $ t * 2 * pi / 3

vshadeA tRaw = tra . rot where
  rot = fromGA . conj r . toGA
  r | even sec2 = rbase
    | otherwise = gMul rbase $ r111 $ t / 2000
  tra [x, y, z] = [x + 2, y + 2, z + 5]
  (sec2, msec2) = (tRaw `mod` 12000) `divMod` 2000
  rs = scanl gMul mv1 $ repeat $ r111 1
  rbase = rs!!div sec2 2
  t = fromIntegral msec2

conj r u = gMul (rotInv r) $ gMul u r

dual = (`gMul` M.singleton "zyx" 1)

toRGB [r, g, b] = RGB r g b

paint vshade canvas t = renderOnTop canvas . f <$> filter vis (zip fs cs)
  where
    ws = vshade (round t) <$> vs
    f (face, c) = color (toRGB c) . fill . path $ scr . pro . (ws!!) <$> face
    -- Flip signs so that x-axis goes right, y-axis goes up,
    -- and z-axis goes out of the screen.
    pro [x, y, z] = [-x / z, y / z]
    scr [x, y] = ((x + 0.5) * 320 + 160, (y - 0.5) * 320 + 160)
    -- Cull faces pointing away from origin.
    vis (face, _) = d!"" <= 0
      where
        (u:v:_) = zipWith (zipWith (-)) (tail ts) ts
        d = gMul (dual (gMul (toGA u) (toGA v))) $ toGA (head ts)
        ts = (ws!!) <$> face

main = withElems ["canvasQ", "canvasA"] $ \[qElem, aElem] -> do
  Just canvasQ <- fromElem qElem
  Just canvasA <- fromElem aElem
  paused <- newIORef False
  let
    anim t = do
      render canvasQ $ pure ()
      render canvasA $ pure ()
      sequence_ $ paint vshadeQ canvasQ t
      sequence_ $ paint vshadeA canvasA t
      b <- readIORef paused
      unless b $ void $ requestAnimationFrame anim

    pause = do
      b <- readIORef paused
      writeIORef paused $ not b
      when b $ void $ requestAnimationFrame anim

  _ <- qElem `onEvent` MouseDown $ const pause
  _ <- aElem `onEvent` MouseDown $ const pause

  requestAnimationFrame anim
