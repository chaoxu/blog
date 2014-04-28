---
title: The Aho–Corasick Automaton in Haskell
---

It is common that one might want to match $k$ different strings against one single text of length $m$. One can of course apply the [KMP algorithm](/posts/2014-04-11-the-kmp-algorithm-in-haskell.html) individually, and result an algorithm that runs in $O(km)$ time.

Faster algorithms are known. The idea is to build an finite state transducer that can output which strings is the suffix of the string it read. The Aho-Corasick automaton is a compressed version of such transducer, as the size does not depend on the size of the alphabet. 

```haskell
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Function
import Control.Arrow
data Automaton a b = Node {delta  :s: a -> Automaton a b,
                           output :: b
                         }

equivalentClasses :: (a->a->Bool)->[a]->[[a]]
equivalentClasses eq = foldl parts []
  where parts [] a = [[a]]
        parts (x:xs) a 
         | eq (head x) a = (a:x):xs
         | otherwise     = x:parts xs a

buildAutomaton :: (Monoid b,Eq a) => [([a],b)] -> Automaton a b
buildAutomaton xs = automaton
  where automaton = build (const automaton) xs mempty

build :: (Monoid b,Eq a)=> (a -> Automaton a b) -> [([a],b)] -> b -> Automaton a b
build trans' xs out = node
  where node  = Node trans out
        trans a
          | isNothing next = trans' a
          | otherwise      = fromJust next
          where next = lookup a table
        table =  map transPair $ equivalentClasses (on (==) (head . fst)) xs
        transPair xs = (a, build (delta (trans' a)) ys out)
         where a  = head $ fst $ head xs
               (ys,zs) = partition (not . null . fst) $ map (first tail) xs
               out = mappend (mconcat $ map snd zs) (output $ trans' a)

match :: Eq a => Automaton a b -> [a] -> [b]
match a xs = map output $ scanl delta a xs

match' :: Eq a => [[a]] -> [a] -> [[[a]]]
match' pat = match (buildAutomaton $ map (\x-> (x,[x])) pat)

isInfixOf' :: Eq a => [a] -> [a] -> Bool
isInfixOf' xs ys = getAll $ mconcat $ match (buildAutomaton [(xs, All True)]) ys
```