import Control.Monad.Random

randomPoly :: Rand StdGen [Integer]
randomPoly = map (`mod` 10) <$> take 4 <$> getRandoms

mkEvenPoly :: Rand StdGen [Integer]
mkEvenPoly = do
  p <- randomPoly
  let res
        | even $ sum p = return p
        | otherwise = mkEvenPoly
  res

-- Use do notation to compose multiple monads into one.
randomStuff :: Rand StdGen (Float, [[Integer]])
randomStuff = do
  f <- getRandom
  p0 <- randomPoly
  p1 <- randomPoly
  e0 <- mkEvenPoly
  e1 <- mkEvenPoly
  e2 <- mkEvenPoly
  return (f, [p0, p1, e0, e1, e2])

-- Use bind operator to accomplish the same thing as do notation.
randomStuff2 :: Rand StdGen (Float, [[Integer]])
randomStuff2 = 
  getRandom  >>= \f ->
  randomPoly >>= \p0 ->
  randomPoly >>= \p1 ->
  mkEvenPoly >>= \e0 ->
  mkEvenPoly >>= \e1 ->
  mkEvenPoly >>= \e2 ->
  return (f, [p0, p1, e0, e1, e2])

main = do
  -- set an initial seed once.
  g0 <- getStdGen 

  -- actually generate the random values.
  let (stuff, g1) = runRand randomStuff g0  

  print stuff

  -- use the same seed to generate the same random values 
  let (stuff, g2) = runRand randomStuff2 g0  

  print stuff
