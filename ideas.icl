implementation module ideas

import Data.Maybe
import StdList

// --- GENERAL HELPER FUNCTIONS

topRules :: a (TTree a) -> [Rule a]
topRules _  Empty               = []
topRules _ (Leaf r)             = [r]
topRules _ (Seq [])             = []
topRules d (Seq       [t:ts])   = case topRules d t of
									[] = topRules d (Seq ts)
									x  = x
topRules d (Choice    t)        = flatten (map (topRules d) t)
topRules d (Parallel t)         = flatten (map (topRules d) t)
topRules d (Condition c t) |c d = topRules d t //we need the state in this case!
						   |otherwise = []
toName :: (Rule a) -> String
toName (Rule n _) = n

score :: (a -> Int) a (Rule a) -> Int
score f d (Rule _ e) = f (e d)
							
sortScore :: [(a,b)] -> [(a,b)] | Ord a
sortScore [(n,r):rs] = sortScore [(ni,ri) \\ (ni,ri)<-rs | ni<=n] ++ [(n,r)] ++ sortScore [(ni,ri) \\ (ni,ri)<-rs | ni>n]
								
// --- IDEAS Services
		
allFirsts :: (TTree a) a -> [String]
allFirsts t d = map toName (topRules d t)



// Fitness-based services
fitnessHint :: (a -> Int) a (TTree a) -> [String]
fitnessHint f d t = map toName (findBest (map (\n -> (n,score f d n)) (topRules d t)))

findBest :: [(Rule a, Int)] -> [Rule a]
findBest [] = []
findBest [(t1,n):xs] = (\(x,_)-> x) (helper1 ([t1],n) xs) //we select the first result as a default candidate

helper1 :: ([Rule a],Int) [(Rule a, Int)] -> ([Rule a],Int)
helper1 a [] = a
helper1 (tl,n) [(t2,n2):xs] | n==n2 = helper1 (tl++[t2],n) xs
							| n<n2  = helper1 (tl,n) xs
							| n>n2  = helper1 ([t2],n2) xs

//Takes a tuple of rule history, maybe tree and state a, and returns an expansion of all top level rules
expand :: ([String], TTree a, a) -> [([String], TTree a, a)]
expand (his, Empty, dom) 		   = []
expand (his, Leaf (Rule n e), dom) = [(his ++ [n],Empty, e dom)]
expand (his, Seq []         , dom) = []
expand (his, Seq [a:as]     , dom) = case expand (his, a, dom) of
										[(nhis, Empty, ndom)] -> [(nhis, Seq as, ndom)]
										[(nhis, na, ndom)] -> [(nhis, Seq [na:as], ndom)]
expand (his, Choice a       , dom) = flatten (map (\t -> expand (his, t, dom)) a)
expand (his, Condition c a  , dom)|c dom = expand (his, a, dom)
								  |otherwise = []
expand (his, Parallel [a:[]], dom) = expand (his, a, dom)
expand (his, Parallel [a:as], dom) = (map (\item -> join item as) (expand (his, a, dom))) ++
												(map (\item -> join item [a]) (expand (his, (Parallel as), dom)))
												where
													join :: ([String], TTree a, a) [TTree a] -> ([String], TTree a, a)
													join (his, Empty, dom) [] = (his, Empty, dom)
													join (his, Empty, dom) [a:[]] = (his, a, dom)
													join (his, Empty, dom) a = (his, (Parallel a), dom)
													join (his, x, dom) a = (his, (Parallel [x:a]), dom)

//getTrace :: (Domain -> Bool) Domain (String -> (Domain -> Domain)) TTree -> [String]
// TODO: write this function. We use firstHint to compute the whole trace to reach the goal.

// --- Brute-force services

bruteForceHint :: (a -> Bool) a (TTree a) -> [String]
bruteForceHint done domain tree = map hd (BFStep done [([],tree,domain)])

getTraceB :: (a -> Bool) a (TTree a) -> [[String]]
getTraceB done domain tree = BFStep done [([],tree,domain)]
	
	
BFStep :: (a -> Bool) [([String], TTree a, a)] -> [[String]]
BFStep done [] = []
BFStep done items = case [h \\ (h,_,d)<-items | done d] //extract expansions that fulfill the goal
					 of [] = BFStep done (flatten (map expand items)) //if there are none, recurse
    			   		x  = x			//otherwise, return this result
					                      
			                      
// --- Heuristic search

heuristicHint :: (a -> Int) (a -> Bool) (TTree a) a -> String
heuristicHint h done t dom = hd (HStep h done [(0,([],t,dom))])

HStep :: (a -> Int) (a -> Bool) [(Int, ([String], TTree a ,a))] -> [String]
HStep heur done [(_,t):xs] = HDecide heur done (sortScore ((map (\(h,t,d) -> (heur d,( h, t,d))) (expand t)) ++ xs))


HDecide :: (a->Int) (a -> Bool) [(Int,([String],TTree a,a))] -> [String]
HDecide heur done [(_,(h,_      ,d)):xns] | done d = h
HDecide heur done [(_,(h,Empty,d)):xns] = HDecide heur done xns
HDecide heur done [(_,(h,t ,d)):xns] = HStep heur done xns								     			