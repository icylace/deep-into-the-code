module O11__PatternMatching where

fst' :: (a, b) -> a
fst' (a, _) = a

snd' :: (a, b) -> b
snd' (_, b) = b

tup :: (Integer, [a]) -> (Integer, [a]) -> (Integer, [a])
tup (a, b) (c, d) = ((a + c), (b ++ d))




-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Key Terms
-- =========
-- Pattern matching: Identifying arguments that follow a given pattern.
