module Assignment04 where

import Prelude hiding (Either(..))

import Control.Applicative(liftA, liftA2, liftA3)
import Data.List(nub)

import FiniteStatePart2

---------------------------------------
-- Setup for section 1

type SLG sy = ([sy], [sy], [sy], [(sy,sy)])
data ConstructedState sy = ExtraState | StateForSymbol sy deriving (Eq, Show)

slg1 :: SLG SegmentCV
slg1 = ([C,V], [C], [V], [(C,C),(C,V),(V,V)])

slg2 :: SLG Int
slg2 = ([1,2,3], [1,2,3], [1,2,3], [(1,1),(2,2),(3,3),(1,2),(2,1),(1,3),(3,1)])

---------------------------------------
-- Setup for section 2

data Either a b = First a | Second b deriving (Show,Eq)

re1 :: RegExp Char
re1 = Concat (Alt (Lit 'a') (Lit 'b')) (Lit 'c')

re2 :: RegExp Char
re2 = Star re1

re3 :: RegExp Int
re3 = Star (Concat ZeroRE (Lit 3)) 

re4 :: RegExp Int
re4 = Concat (Alt (Lit 0) (Lit 1)) (Star (Lit 2))

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

{-*Assignment04> generatesSLG slg1 [C,C,V]
True
*Assignment04> generatesSLG slg1 [C,V]
True
*Assignment04> generatesSLG slg1 [V]
False
*Assignment04> generatesSLG slg1 [V,C]
False
*Assignment04> generatesSLG slg2 [1]
True
*Assignment04> generatesSLG slg2 [1,2,1,2,1,3]
True
*Assignment04> generatesSLG slg2 [1,2,1,2,1,3,2]
False
*Assignment04> generatesSLG slg2 []
False-}

backwardSLG :: (Eq sy) => SLG sy -> [sy] -> Bool 
backwardSLG m w = 
    let (syms, i, f, bigrams) = m in
    case w of  
        [] -> False
        x:rest -> case rest of
            [] -> elem x f
            (x2:rest2) -> elem (x,x2) bigrams && backwardSLG m rest


generatesSLG :: (Eq sy) => SLG sy -> [sy] -> Bool
generatesSLG m w = 
    let (syms, i, f, bigrams) = m in 
        case w of
            [] -> False
            x:rest -> elem x i && backwardSLG m w

{-*Assignment04> generates (slgToFSA slg1) [C,C,V] 
True
*Assignment04> generates (slgToFSA slg1) [V,C]
False
*Assignment04> generates (slgToFSA slg2) [1,2,1,2,1,3] 
True
*Assignment04> generates (slgToFSA slg2) []
False
*Assignment04> generates (slgToFSA (["mwa","ha"],["mwa"],["ha"],[("mwa","ha"),("ha","ha")])) ["mwa","mwa"]
False
*Assignment04> generates (slgToFSA (["mwa","ha"],["mwa"],["ha"],[("mwa","ha"),("ha","ha")])) ["mwa","ha","ha"] 
True -}

syToStatelist l =
  case l of
    [] -> []
    x:rest -> [StateForSymbol x] ++ syToStatelist rest

makeState l =
  case l of
    []->[]
    x:rest -> let (y, z) = x in [(StateForSymbol y, z, StateForSymbol z)] ++ makeState rest

slgToFSA :: SLG sy -> Automaton (ConstructedState sy) sy
slgToFSA (syms, i, f, bigrams) = 
    let states = [ExtraState] ++ syToStatelist syms in
    let first = (ExtraState, head i, (StateForSymbol(head i))) in
    let i' = [ExtraState] in
    let f' =  syToStatelist f in
    let delta = [first] ++ makeState bigrams in
        (states, syms, i', f', delta)

{-*Assignment04> generates (removeEpsilons (unionFSAs efsa_handout19 efsa_xyz)) "abbb" 
True
 *Assignment04> generates (removeEpsilons (unionFSAs efsa_handout19 efsa_xyz)) "abbbbb" 
 False
*Assignment04> generates (removeEpsilons (unionFSAs efsa_handout19 efsa_xyz)) "xxxyz" 
True
*Assignment04> generates (removeEpsilons (unionFSAs efsa_handout19 efsa_xyz)) "abbbxxxyz" 
False -}

unionFSAs :: (Eq sy) => EpsAutomaton st1 sy -> EpsAutomaton st2 sy -> EpsAutomaton (Either st1 st2) sy
unionFSAs m m' =
    let (states, syms, i, f, delta) = m in
    let (states', syms', i', f', delta') = m' in
    let states1 = map (\x -> First x) states in
    let states2 = map (\x -> Second x) states' in
    let newStates = states1 ++ states2 in
    let newSyms = nub (syms ++ syms') in
    let newi = map (\x -> First x) i ++ map (\x -> Second x) i' in
    let newf = map (\x -> First x) f ++ map (\x -> Second x) f' in
    let newDelta = map (\(a,b,c) -> (First a, b, First c)) delta ++ map (\(a,b,c) -> (Second a, b, Second c)) delta' in
        (newStates, newSyms, newi, newf, newDelta)

{-*Assignment04> generates (removeEpsilons (concatFSAs efsa_handout19 efsa_xyz)) "abbbxxxyz"
True
*Assignment04> generates (removeEpsilons (concatFSAs efsa_handout19 efsa_xyz)) "xxxyzabbb"
False -}

concatFSAs :: (Eq sy) => EpsAutomaton st1 sy -> EpsAutomaton st2 sy -> EpsAutomaton (Either st1 st2) sy
concatFSAs m m' =
    let (states, syms, i, f, delta) = m in
    let (states', syms', i', f', delta') = m' in
    let states1 = map (\x -> First x) states in
    let states2 = map (\x -> Second x) states' in
    let newStates = states1 ++ states2 in
    let newSyms = nub (syms ++ syms') in
    let newi = map (\x -> First x) i in
    let newf = map (\x -> Second x) f' in
    let firstEnd = map (\x -> First x) f in
    let secondStart = map (\x -> Second x) i' in
    let connectFSAs = liftA2 (\x -> \y -> (x, Nothing, y)) firstEnd secondStart in --creating epsilon transition between FSAs
    let newDelta = connectFSAs ++ map (\(a,b,c) -> (First a, b, First c)) delta ++ map (\(a,b,c) -> (Second a, b, Second c)) delta' in 
        (newStates, newSyms, newi, newf, newDelta)

{-*Assignment04> generates (removeEpsilons (starFSA efsa_handout19)) ""
True
*Assignment04> generates (removeEpsilons (starFSA efsa_handout19)) "bbbbb" True
*Assignment04> generates (removeEpsilons (starFSA efsa_handout19)) "bbabbb" True
*Assignment04> generates (removeEpsilons (starFSA efsa_handout19)) "aaab" False -}

starFSA :: EpsAutomaton st sy -> EpsAutomaton (Either Int st) sy
starFSA m =
    let (states, syms, i, f, delta) = m in
    let states2 = map (\x -> Second x) states in
    let newStates = [(First 1)] ++ states2 in
    let f2 = map (\x -> Second x) f in
    let i2 = map (\x -> Second x) i in
    let newfs = [(First 1)] ++ f2 in
    let newi = [(First 1)] in
    let connectNewToOriginal = liftA2 (\x -> \y -> (x, Nothing, y)) [(First 1)] i2 in    
    let connectFinalToInitial = liftA2 (\x -> \y -> (x, Nothing, y)) f2 i2 in
    let newDelta = (map (\(a,b,c) -> (Second a, b, Second c)) delta) ++ connectFinalToInitial ++ connectNewToOriginal in
        (newStates, syms, newi, newfs, newDelta)
    
flatten :: Either Int Int -> Int
flatten x = case x of
    First a -> a * 2
    Second a -> a * 2 + 1

mapStates :: (a -> b) -> EpsAutomaton a sy -> EpsAutomaton b sy
mapStates f e =
    let (states, symbols, i, end, delta) = e in
    let newStates = map f states in
    let newInitial = map f i in
    let newEnd = map f end in
    let newDelta = [(f q1, x, f q2) | (q1, x, q2) <- delta] in
    (newStates, symbols, newInitial, newEnd, newDelta)


{-
*Assignment04> generates (removeEpsilons (reToFSA re2)) "acacbcac" 
True
*Assignment04> generates (removeEpsilons (reToFSA re2)) "acacbca" 
False
*Assignment04> generates (removeEpsilons (reToFSA re3)) [] 
True
*Assignment04> generates (removeEpsilons (reToFSA re3)) [3] 
False
*Assignment04> generates (removeEpsilons (reToFSA re4)) [0,2,2,2] 
True
*Assignment04> generates (removeEpsilons (reToFSA re4)) [1,2,2] 
True
*Assignment04> generates (removeEpsilons (reToFSA re4)) [0,1,2,2,2,2,2]
False
*Assignment04> generates (removeEpsilons (reToFSA (Star re4))) [0,1,2,2,2,2,2] 
True -}

reToFSA :: (Eq sy) => RegExp sy -> EpsAutomaton Int sy
reToFSA r = case r of
    Lit r1 -> ([0,1], [r1], [0], [1], [(0, Just r1, 1)])
    Alt r1 r2 -> mapStates flatten (unionFSAs (reToFSA r1) (reToFSA r2))
    Concat r1 r2 -> mapStates flatten (concatFSAs(reToFSA r1) (reToFSA r2))
    Star r1 -> mapStates flatten (starFSA (reToFSA r1))
    ZeroRE -> ([0], [], [0], [], []) --empty set
    OneRE -> ([0,1], [], [0], [1], [(0, Nothing, 1)]) --empty string