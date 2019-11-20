definition module ideas

import Data.Maybe

// --- Types

:: TTree a = Seq [TTree a]
		   | Choice [TTree a]
		   | Parallel [TTree a]
		   | Condition (a -> Bool) (TTree a)
		   | Leaf (Rule a)
		   | Empty
		   

:: Rule a = Rule String (Effect a)

:: Effect a :== a -> a						

// --- IDEAS Services

allFirsts :: (TTree a) a -> [String]

// ---- Fitness-based services
fitnessHint :: (a -> Int) a (TTree a) -> [String]

// --- Brute-force based services
bruteForceHint :: (a -> Bool) a (TTree a) -> [String]

// --- Heuristic search
heuristicHint :: (a -> Int) (a -> Bool) (TTree a) a -> String