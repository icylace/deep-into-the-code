module O29__State where

import System.Random (StdGen, mkStdGen, next, random, randomR)

_ = mkStdGen 0    -- `1 1`

{-
> :t mkStdGen 0
mkStdGen 0 :: StdGen
-}

sg :: StdGen
sg = mkStdGen 0

x :: (Int, StdGen)
x = next sg    -- `(2147482884, 40014 40692)`

y :: (Int, StdGen)
y = next $ snd x    -- `(2092764894, 1601120196 1655838864)`

y':: (Int, StdGen)
y' = next $ snd $ next $ snd x    -- `(1390461064, 1346387765 2103410263)`

-- -----------------------------------------------------------------------------

-- `random` can generate values of different types, so we need to specify the
-- type to use.

{-
> :t random
random :: (Random a, RandomGen g) => g -> (a, g)
-}

z = random $ snd x :: (Int, StdGen)
z' = random $ snd x :: (Double, StdGen)

{-
z'' = random $ snd x    -- Error.
-}

{-
> :t randomR
randomR :: (Random a, RandomGen g) => (a, a) -> g -> (a, g)
-}

z1 = randomR (0, 3) $ snd x :: (Int, StdGen)        -- `(1, 1601120196 1655838864)`
z1' = randomR (0, 3) $ snd x :: (Double, StdGen)    -- `(1.259762189189801, 439883729 1872071452)`

rx :: (Int, StdGen)
rx = random $ snd x    -- `(138890298504988632, 439883729 1872071452)`

-- -----------------------------------------------------------------------------

-- This chaining of state can get tedious. `State` is intended to address it.

newtype State s a = State { runState :: s -> (a, s) }

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

{-

Key Terms
=========

State: Values which may vary during the course of program execution.

-}
