import Criterion.Main
import Control.Monad
import Control.Monad.Par
import Data.Monoid
import Debug.Trace
import Text.Printf

-- * Code for playing with Monad.Par and Criterion
--
--   Compile using -threaded -rtsops, run with -r /dev/stdout +RTS -N

-- Takeaways:
--  * defer(P) is ok, a few percents slower than manual IVar handling

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- | Sugar functions to avoid working with IVars directly.
defer :: (NFData a) => Par a -> Par (Par a)
defer = liftM get . spawn
deferP :: (NFData a) => a -> Par (Par a)
deferP = liftM get . spawnP

summary a b c = a + b + c

linear :: Int -> Int
linear x =
  let a = fib x
      b = fib (x + 1)
      c = a + b
  in summary a b c

-- | Alternatives for shooting up stuff in parallel
parFoldMap :: (Num a, NFData a) => (a -> Par a) -> [a] -> Par a
parFoldMap f xs = join $ (fmap sum . sequence) `fmap` mapM (defer . f) xs

parFoldMap2 :: (Num a, NFData a) => (a -> a) -> [a] -> Par a
parFoldMap2 f xs = do
  pars <- mapM (deferP . f) xs
  xs <- sequence pars
  return (sum xs)

-- | Original version, with lots of direct IVar access
calc0 :: Int -> Par Int
calc0 x = do
  ia <- spawnP $ fib x
  ib <- spawnP $ fib (x + 1)
  ic <- spawn $ liftM2 (+) (get ia) (get ib)
  liftM3 summary (get ia) (get ib) (get ic)

-- | Sugared version
calc1 :: Int -> Par Int
calc1 x = do
  pa <- deferP $ fib x
  pb <- deferP $ fib (x + 1)
  pc <- defer $ liftM2 (+) pa pb
  liftM3 summary pa pb pc

t = 28
c = 6
i = take c (repeat t)

-- | Funs for testing that spawn/defer indeed starts the computation in
--   parallel early.
ex x = do
  ia <- spawnP (trace "starting1" $ fib x)
  ib <- spawnP (trace "starting2" $ fib (x-1))
  ics <- mapM spawnP [(trace ("starting3_" ++ show i) $
           fib (x-8+i)) |  i <- [0..10]]
  liftM2 sum (get' "1" ia) (get' "2" ib)
  where
    get' s = trace ("get"++s) . get
    sum a b = trace "summing" $ a + b

ex2 x = do
  pa <- deferP' "1" (trace "starting1" $ fib x)
  pb <- deferP' "2" (trace "starting2" $ fib (x-1))
  pcs <- mapM deferP [(trace ("starting3_" ++ show i) $
           fib (x-8+i)) |  i <- [0..10]]
  liftM2 sum pa pb
  where
    deferP' s = liftM (get' s) . spawnP
    get' s = trace ("get"++s) . get
    sum a b = trace "summing" $ a + b


main' = print$ runPar (ex2 38)

main'' = do
  print $ (runPar . calc0) t
  print $ (runPar . calc1) t
  print $ linear t

main = defaultMain
  [ bcompare
    [ bench "linear" $ nf (sum . map linear) i
    , bench "calc0" $ nf (runPar . parFoldMap calc0) i
    , bench "calc1" $ nf (runPar . parFoldMap calc1) i
    , bench "calc0_2" $ nf (runPar . parFoldMap2 (runPar . calc0)) i
    , bench "calc1_2" $ nf (runPar . parFoldMap2 (runPar . calc1)) i
    ]
  ]
