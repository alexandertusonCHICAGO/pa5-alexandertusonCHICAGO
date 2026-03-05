import Control.Monad
import qualified Data.Map.Strict as M
import Data.List
import Data.Array

newtype Probability a =
  Probability { getProbabilities :: [(a, Double)] }
  deriving Show


instance Functor Probability where
  fmap f (Probability xs) =
    Probability [(f x, p) | (x,p) <- xs]


instance Applicative Probability where
  pure x = Probability [(x,1)]

  Probability fs <*> Probability xs =
    Probability [(f x, pf * px) | (f,pf) <- fs, (x,px) <- xs]


instance Monad Probability where
  return = pure

  Probability xs >>= f =
    Probability
      [ (y, p * py)
      | (x,p) <- xs
      , (y,py) <- getProbabilities (f x)
      ]


roll :: Probability Int
roll = Probability [(i, 1/6) | i <- [1..6]]


normalize :: Ord a => Probability a -> Probability a
normalize (Probability xs) =
  Probability (M.toList (M.fromListWith (+) xs))


twoRolls :: Probability Int
twoRolls = do
  a <- roll
  b <- roll
  pure (a + b)


rollDice :: Int -> Probability Int
rollDice 0 = pure 0
rollDice n =
  normalize $ do
    r <- roll
    s <- rollDice (n-1)
    pure (r + s)


mode :: Probability a -> a
mode (Probability xs) =
  fst $ maximumBy (\(_,p1) (_,p2) -> compare p1 p2) xs


rollingStones :: Array Int (Probability Int)
rollingStones = arr
  where
    arr = listArray (0,50) [f n | n <- [0..50]]

    f 0 = pure 0
    f n = normalize $ do
      r <- roll
      let next = max 0 (n - r)
      k <- arr ! next
      pure (k + 1)


firstPlayerWinProb :: Int -> Double
firstPlayerWinProb n =
  sum [p | (k,p) <- getProbabilities (rollingStones ! n), odd k]


main :: IO ()
main = do
  putStrLn "Normalized distribution for two dice:"
  print (normalize twoRolls)

  putStrLn "\nMode of rollDice 100:"
  print (mode (rollDice 100))

  putStrLn "\nFirst player win probabilities:"
  mapM_ printResult [2..50]

printResult :: Int -> IO ()
printResult n =
  putStrLn (show n ++ ": " ++ show (firstPlayerWinProb n * 100) ++ "%")