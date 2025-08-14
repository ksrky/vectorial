module Vectorial.Projection where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Logic
import Data.Complex
import Data.Functor.Identity
import System.Random
import Vectorial.Vector.Internal qualified as V

type Weight = Double

-- | Weighted LogicT
type WLogicT m a = LogicT m (Weight, a)

type WLogic a = WLogicT Identity a

observeRandomT :: MonadIO m => WLogicT m a -> m a
observeRandomT plt = do
    res <- observeAllT plt
    liftIO $ probChoice res
  where
    probChoice :: [(Weight, a)] -> IO a
    probChoice [] = fail "No answer"
    probChoice [(_, x)] = return x
    probChoice items = do
        let total = sum (map fst items)
            items' = map (\(p, x) -> (p / total, x)) items
        r <- randomRIO (0.0, 1.0)
        select r items'

    select :: MonadFail m => Double -> [(Weight, a)] -> m a
    select r = go 0.0
      where
        go _ [] = fail "No answer"
        go acc ((p, x) : xs)
            | r <= acc + p = return x
            | otherwise = go (acc + p) xs

runV :: Monad m => V.V a -> WLogicT m a
runV (V.V xs) = do
    let xs' = map (\(V.CC c, x) -> (magnitude c * magnitude c, x)) xs
    foldr (\(p, x) acc -> pure (p, x) <> acc) empty xs'

simulate :: V.V a -> IO a
simulate = observeRandomT . runV

observe :: Monad m => V.V a -> m [(Weight, a)]
observe = observeAllT . runV
