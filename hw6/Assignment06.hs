{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}

module Assignment06 where

import Control.Applicative(liftA, liftA2, liftA3)

import ContextFree

data Tree nt t = Leaf nt t | NonLeaf nt (Tree nt t) (Tree nt t) deriving Show

tree1 :: Tree Cat String
tree1 = NonLeaf VP (NonLeaf VP (Leaf V "watches") (Leaf NP "spies"))
                   (NonLeaf PP (Leaf P "with") (Leaf NP "telescopes"))

tree2 :: Tree Cat String
tree2 = NonLeaf VP (Leaf V "watches")
                   (NonLeaf NP (Leaf NP "spies") (NonLeaf PP (Leaf P "with") (Leaf NP "telescopes")))
------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.


leftmost :: Tree nt t -> [RewriteRule nt t]
leftmost tree =
    case tree of
        Leaf nonterm term -> [TRule nonterm term]
        NonLeaf nonterm l1 r1 ->
            let
                rootL = case l1 of
                    Leaf nontermL term -> nontermL
                    NonLeaf nontermL l1 r1 -> nontermL

                rootR = case r1 of
                    Leaf nontermR term -> nontermR
                    NonLeaf nontermR l1 r1 -> nontermR
            in
                [NTRule nonterm (rootL, rootR)] ++ leftmost l1 ++ leftmost r1



{- 
*Assignment06> f cfg12 (words "watches spies with telescopes") 
True
*Assignment06> f cfg12 (words "spies with telescopes")
True
*Assignment06> f cfg12 (words "telescopes with telescopes") False
*Assignment06> f cfg12a (words "watches spies with telescopes") 
1.2e-2
*Assignment06> f cfg12a (words "telescopes with telescopes") 
0.0
*Assignment06> f cfg12b (words "watches spies with telescopes") 
6.0e-3
*Assignment06> f cfg12b (words "telescopes with telescopes")
9.0e-3
*Assignment06> f cfg12b (words "telescopes with telescopes with spies with spies") 
7.200000000000002e-5
-}

f :: (Ord nt, Ord t, Semiring a) => GenericCFG nt t a -> [t] -> a
f c l = let (nts, ts, i, rules) = c in
  gen_or (map (\nt -> (i nt) &&& fastInside c l nt) nts)
  



outside :: (Ord nt, Ord t, Semiring v) => GenericCFG nt t v ->
                              ([t],[t]) -> nt -> v
outside cfg (ys,zs) n =
    let (nts, ts, i, r) = cfg in
    case (null ys && null zs) of
        True -> i n 
        False ->
                let
                    conj1 i p rd = (outside cfg (ys, drop i zs) p) &&& r (NTRule p (n, rd)) &&& (fastInside cfg (take i zs) rd)

                    conj2 i p ld = (outside cfg (take i ys, zs) p) &&& r (NTRule p (ld, n)) &&& (fastInside cfg (drop i ys) ld)
            
                in
                gen_or(liftA3 conj1 [1 .. (length zs)] nts nts) ||| gen_or(liftA3 conj2 [0 .. (length ys - 1)] nts nts) 

                








