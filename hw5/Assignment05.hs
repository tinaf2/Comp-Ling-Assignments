{-# LANGUAGE FlexibleInstances #-}

module Assignment05 where

import Control.Applicative(liftA, liftA2, liftA3)

import SemiringFSA

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

backward :: (Semiring v) => GenericAutomaton st sy v -> [sy] -> st -> v
backward g w q = 
    let (states, syms, i, f, delta) = g in
    case w of 
        [] -> f q
        x:rest -> gen_or (map (\q1 -> delta (q,x,q1) &&& backward g rest q1) states)

f :: (Semiring v) => GenericAutomaton st sy v -> [sy] -> v
f g w =
    let (states, syms, i, f, delta) = g in
    gen_or (map (\q0 -> i q0 &&& backward g w q0) states)

addCosts :: Cost -> Cost -> Cost
addCosts x y = 
        case x of 
            Inf -> Inf
            TheInt a -> case y of 
                            Inf -> Inf
                            TheInt b -> TheInt (a+b)

{- *Assignment05> minCost (TheInt 3) (TheInt 4)
TheInt 3
*Assignment05> minCost (TheInt 3) Inf
TheInt 3
*Assignment05> minCost Inf (TheInt 4)
TheInt 4
*Assignment05> minCost Inf Inf
Inf -}
minCost :: Cost -> Cost -> Cost
minCost x y = 
    let TheInt a = x in
    let TheInt b = y in
    case x of
        Inf -> case y of
                Inf -> Inf
                _ -> TheInt b
        _ -> case y of 
                Inf -> TheInt a
                _ -> case a==b of 
                    True -> TheInt a
                    False -> case a>b of 
                                True -> TheInt b
                                False -> TheInt a

{-*Assignment05> gen_and [TheInt 0, TheInt 5, TheInt 0, TheInt 0, TheInt 0] 
TheInt 5
*Assignment05> gen_or [TheInt 5, TheInt 7, Inf]
TheInt 5
*Assignment05> f gfsa32 "VCV"
TheInt 1
*Assignment05> f gfsa32 "CVCVCV"
TheInt 0
*Assignment05> f gfsa32 "CVCVCVC"
TheInt 2
*Assignment05> f gfsa32 "VCVCVC"
TheInt 3
*Assignment05> f gfsa32 "VCVCVCV"
TheInt 1
*Assignment05> f gfsa32 "VVVV"
TheInt 4
*Assignment05> f gfsa32 "CC"
Inf -}

instance Semiring Cost where
    x &&& y = addCosts x y
    x ||| y = minCost x y
    gtrue = TheInt 0
    gfalse = Inf

{-*Assignment05> ["hello","world"] ||| ["foo","bar"] 
["hello","world","foo","bar"]
*Assignment05> ["hello","world"] &&& ["foo","bar"] 
["hellofoo","hellobar","worldfoo","worldbar"] 
*Assignment05> ["hello","world"] ||| [] 
["hello","world"]
*Assignment05> ["hello","world"] &&& []
[]
*Assignment05> ["hello","world"] &&& [""]
["hello","world"]
*Assignment05> gen_and [["tic","tac","toe"],["do","re","mi"],["foo"]] 
["ticdofoo","ticrefoo","ticmifoo","tacdofoo","tacrefoo","tacmifoo","toedofoo","
toerefoo","toemifoo"]
*Assignment05> gen_or [["tic","tac","toe"],["do","re","mi"],["foo"]]
["tic","tac","toe","do","re","mi","foo"] -}

{-instance Semiring [[a]] where
    x &&& y = liftA2 (\a -> \b -> a ++ b) x y
    x ||| y = x ++ y
    gtrue = case (undefined :: a) of
                Char -> [""]
                Integer -> [[]]
    gfalse = []-}

instance Semiring [[Char]] where
    x &&& y = liftA2 (\a -> \b -> a ++ b) x y
    x ||| y = x ++ y
    gtrue = [""]
    gfalse = []

instance Semiring [[Integer]] where
    x &&& y = liftA2 (\a -> \b -> a ++ b) x y
    x ||| y = x ++ y
    gtrue = [[]]
    gfalse = []

instance Semiring [[Int]] where
    x &&& y = liftA2 (\a -> \b -> a ++ b) x y
    x ||| y = x ++ y
    gtrue = [[]]
    gfalse = []    

{-*Assignment05> f gfsa34 "VCV"
["VCV","VCVV","VV"]
*Assignment05> f gfsa34 "CV"
["CV","CVV"]
*Assignment05> f gfsa34 "VC"
["V"]
*Assignment05> backward gfsa34 "VCV" 1
["VCV","VCVV","VV"]
*Assignment05> backward gfsa34 "VCV" 2 
["VCV","VCVV","VVCV","VVCVV","VV","VVV"] 
*Assignment05> backward gfsa34 "VCV" 3 
[]-}
gfsa34 :: GenericAutomaton Int Char [[Char]]
gfsa34 = makeGFSA [] ([1,2,3], ['C','V'],[(1, [""])], [(1, [""])], [((1, 'V', 1), ["V"]), ((1, 'C', 2), ["C"]), ((2,'V',1), ["V", "VV"]), ((2,'V',3), ["V", "VV"]), ((3,'C',1), [""]), ((1,'V',3), ["V"])])

{-*Assignment05> f gfsa_flap "atan"
["atan","aTan"]
*Assignment05> f gfsa_flap "atat"
["atat","aTat"]
*Assignment05> f gfsa_flap "atnat"
["atnat"]
*Assignment05> f gfsa_flap "tatatnat"
["tatatnat","taTatnat"] -}

gfsa_flap :: GenericAutomaton Int Char [[Char]]
gfsa_flap = makeGFSA [] ([1,2,3], ['a','n','t','T'], [(1, [""])], [(1, [""]), (2, [""]), (3, ["t"])], [((1, 'n', 1), ["n"]), ((1, 't', 1), ["t"]), ((1, 'a', 2), ["a"]), ((2, 'n', 1), ["n"]), ((2, 'a', 2), ["a"]), ((2, 't', 3), [""]), ((3,'a',2), ["ta","Ta"]), ((3,'n',1), ["tn"]), ((3,'t',1), ["tt"])])

{-*Assignment05> f gfsa5_count "VC"
1.0
*Assignment05> f gfsa5_count "CC"
0.0
*Assignment05> f gfsa5_count "VCV"
2.0
*Assignment05> f gfsa5_count "CVVCV"
2.0
*Assignment05> f gfsa5_count "VCVCV"
4.0 -}
gfsa5_count :: GenericAutomaton Int Char Double
gfsa5_count = makeGFSA 0 ([1,2,3], ['C','V'],
                         [(1, 1.0)], [(1, 1.0)], 
                         [((1,'V',1), 1.0),
                          ((1,'C',2), 1.0),
                          ((1,'V',3), 1.0),
                          ((2,'V',1), 1.0),
                          ((2,'V',3), 1.0),
                          ((3,'C',1), 1.0)])

gfsa5_paths :: GenericAutomaton Int Char [[Int]]
gfsa5_paths = makeGFSA [] ([1,2,3], ['C','V'], [(1,[[]])], [(1,[[1]])], 
                         [((1,'V',1), [[1]]),
                          ((1,'C',2), [[1]]),
                          ((1,'V',3), [[1]]),
                          ((2,'V',1), [[2]]),
                          ((2,'V',3), [[2]]),
                          ((3,'C',1), [[3]])])