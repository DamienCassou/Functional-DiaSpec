module Main where

-- context C1 as Boolean {
--   <pull self; ; push self>
-- }

-- Java version:
--  abstract class AbstractC1 { Boolean get(); }

-- Haskell version:
data AbstractC1 = AbstractC1 {get :: () -> Bool}

-- Example implementation
C1 {
  get = true
  }
  

-- context C2 as Boolean {  
-- <pull self; pull C1 ; push self>
-- }

-- Boolean get(C1Closure c1);

data AbstractC2 = AbstractC2 {get :: (() -> Boolean) -> 


