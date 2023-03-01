import Control.Monad
import Control.Monad.ST
import Control.Applicative ((<$>),(<*>))
import Data.STRef

main = do
  print $ twd 10 
  print $ tdw 10 
  print $ twd 0 
  print $ tdw 0 


twd :: Int -> (Int,[Int])
twd m = runST $ do
  ri <- newSTRef 0
  rl <- newSTRef []
  whileDo ( (m>) <$> (readSTRef ri) ) $ do
    i <- readSTRef ri
    modifySTRef rl (i:)
    writeSTRef ri (i+1)

  (,) <$> readSTRef ri <*> readSTRef rl


tdw :: Int -> (Int,[Int])
tdw m = runST $ do
  ri <- newSTRef 0
  rl <- newSTRef []
  doWhile (
    do
      i <- readSTRef ri
      modifySTRef rl (i:)
      writeSTRef ri (i+1)
    ) $ (m>) <$> (readSTRef ri)
    
  (,) <$> readSTRef ri <*> readSTRef rl


whileDo :: ST s Bool -> ST s () -> ST s ()
whileDo fcond fbody = loop
  where
    loop = do
      c <- fcond
      if c
        then fbody >> loop
        else return ()


doWhile :: ST s () -> ST s Bool -> ST s ()
doWhile fbody fcond = loop
  where
    loop = do
      fbody
      c <- fcond
      if c
        then loop
        else return ()

