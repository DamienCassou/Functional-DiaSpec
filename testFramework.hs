module Main where

-- context C1 as Boolean {
--   <pull self; ; push self>
-- }

-- Java version:
--  abstract class AbstractC1 { Boolean get(); }

-- Haskell version:
data AbstractC1 = AbstractC1 {abstractC1_get :: () -> Bool}

-- context C2 as Boolean {  
-- <pull self; pull C1 ; push self>
-- }

-- Boolean get(C1Closure c1);

data AbstractC2 = AbstractC2 {abstractC2_get :: (() -> Bool) -> Bool}

-- example implementation which negates the value returned by c1. Is
-- it possible to make that simpler? By merging the definitions?

myabstractC2_get :: (() -> Bool) -> Bool
myabstractC2_get c1Closure = not (c1Closure ())

myC2 = AbstractC2 myabstractC2_get

-- context C1Indexed as Boolean indexed by val as Integer {
--   <pull self; ; push self>
-- }

-- Boolean get(Integer val)

data AbstractC1Indexed = AbstractC1Indexed {abstractC1Indexed_get :: Integer -> Bool}

-- context C2Indexed as Boolean indexed by val as Integer {
--   <pull self; pull C1Indexed; push self>
-- }

-- Boolean get(C1Closure c1)

data AbstractC2Indexed = AbstractC2Indexed {abstractC2Indexed_get :: Integer -> (Integer -> Bool) -> Bool}


