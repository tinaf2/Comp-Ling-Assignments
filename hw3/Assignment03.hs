module Assignment03 where

-- Imports everything from the FiniteState module
import FiniteState

-- A type we'll use as symbols for some FSAs
data SegmentPKIU = P | K | I | U | MB deriving (Show,Eq)

-- A list-like type that will be useful for computing forward values
data SnocList a = ESL | (SnocList a) ::: a deriving Show

-- The word ``hello'' encoded as a snoc list of characters
sl :: SnocList Char
sl = ((((ESL ::: 'h') ::: 'e') ::: 'l') ::: 'l') ::: 'o'

-- Checks that all states and symbols mentioned in the transition 
-- table (i.e. delta) come from the provided lists of states and symbols.
fsaSanityCheck :: (Eq a) => Automaton a -> Bool
fsaSanityCheck m =
    let (states, syms, i, f, delta) = m in
    let validTransition (q1,x,q2) = elem q1 states && elem x syms && elem q2 states in
    and (map validTransition delta)

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

fsa_countVs :: Automaton SegmentCV
fsa_countVs = ([54, 73, 21, 38], [C,V], [54], [38], [(54, C, 54), (54, V, 73), (73, C, 73), (73, V, 21), (21, C, 21), (21, V, 38), (38, C, 38), (21, V, 54)])


addToFront :: a -> SnocList a -> SnocList a
addToFront x l =  case l of 
                    ESL -> ESL ::: x
                    rest ::: y -> addToFront x rest ::: y


toSnoc :: [a] -> SnocList a
toSnoc l = toSnocHelper (reverse l) 


toSnocHelper :: [a] -> SnocList a
toSnocHelper l = case l of 
                        [] -> ESL
                        x:rest -> toSnocHelper rest ::: x


forward :: (Eq a) => Automaton a -> SnocList a -> State -> Bool
forward m w q = let (states,syms,i,f,delta) = m in
                  case w of 
                    ESL -> elem q i 
                    rest ::: x -> or(map(\q' -> elem (q',x,q) delta && forward m rest q') states)


fsa_twoVs :: Automaton SegmentCV
fsa_twoVs = ([1, 2, 3], [C, V], [1], [3], [(1, C, 1), (1, V, 2), (2, C, 2), (2, V, 3), (3, V, 3), (3, C, 3)])


{-*Assignment03> fsaSanityCheck fsa_thirdlastC
True
*Assignment03> generates fsa_thirdlastC [C,C,C,C,C]
True
*Assignment03> generates fsa_thirdlastC [C,C,C,V,C] 
True
*Assignment03> generates fsa_thirdlastC [C,C,V,V,C] 
False
*Assignment03> generates fsa_thirdlastC [C,C,V]
True
*Assignment03> generates fsa_thirdlastC [C,V]
False-}

fsa_thirdlastC :: Automaton SegmentCV
fsa_thirdlastC = ([1, 2, 3, 4], [C, V], [1], [4], [(1, C, 1), (1, V, 1), (1, C, 2), (2, C, 3), (2, V, 3), (3, C, 4), (3, V, 4)])


{-*Assignment03> fsaSanityCheck fsa_oddEven
True
*Assignment03> generates fsa_oddEven [C]
True
*Assignment03> generates fsa_oddEven [C,V,V]
True
*Assignment03> generates fsa_oddEven [C,C,V]
False
*Assignment03> generates fsa_oddEven [C,C,V,C,V] 
True-}

fsa_oddEven :: Automaton SegmentCV
fsa_oddEven = ([1, 2, 3, 4], [C, V], [1], [2], [(1, C, 2), (2, C, 1), (2, V, 3), (3, V, 2), (3, C, 4), (4, C, 3), (1, V, 4), (4, V, 1)])


{-*Assignment03> fsaSanityCheck fsa_harmony
True
*Assignment03> generates fsa_harmony [P,K,I,K,MB,U,P,U] True
*Assignment03> generates fsa_harmony [P,K,I,K,U,P,U] False
*Assignment03> generates fsa_harmony [K,I,P,I]
True
*Assignment03> generates fsa_harmony [K,P,P,P] True
*Assignment03> generates fsa_harmony [K,I,P,U] False
*Assignment03> generates fsa_harmony [K,I,MB,P,U]
True
*Assignment03> generates fsa_harmony [MB,MB,K,MB,P] True-}

fsa_harmony :: Automaton SegmentPKIU
fsa_harmony = ([1, 2, 3, 4], [P, K, I, U, MB], [1], [1, 3, 4], [(1, K, 2), (1, P, 2), (1, MB, 2), (2, MB, 1), (2, K, 1), (2, P, 1), (2, I, 3), (3, MB, 2), (3, P, 3), (3, K, 3), (3, I, 3), (2, K, 2), (2, P, 2), (2, MB, 2), (2, U, 4), (4, P, 4), (4, K, 4), (4, U, 4),(4, MB, 2), (2, P, 1), (2, K, 1), (2, MB, 1), (2, K, 2), (2, P, 2), (2, MB, 2)])


{-*Assignment03> fsaSanityCheck fsa_MBU
True
*Assignment03> generates fsa_MBU [MB,U]
True
*Assignment03> generates fsa_MBU [U,MB]
False
*Assignment03> generates fsa_MBU [MB]
True
*Assignment03> generates fsa_MBU [MB,K,K,K,K,K,K,U] 
True
*Assignment03> generates fsa_MBU [K,K,K,K,K,K,K,U]
False
*Assignment03> generates fsa_MBU [K,K,K,K,K,K,K,K] 
True
*Assignment03> generates fsa_MBU [MB,I,I,I]
True
*Assignment03> generates fsa_MBU [MB,U,U,U,U,U]
True-}

fsa_MBU :: Automaton SegmentPKIU
fsa_MBU = ([1, 2], [P, K, I, U, MB], [1], [1], [(1, MB, 2), (2, U, 2), (2, U, 1), (1, K, 1), (1, P, 1), (1, I, 1), (1, MB, 1)])


{-*Assignment03> generates (requireVs 2) [V,V]
True
*Assignment03> generates (requireVs 2) [C,V]
False
*Assignment03> generates (requireVs 2) [V,C,V]
True
*Assignment03> generates (requireVs 2) [V,C,V,V] 
False
*Assignment03> generates (requireVs 2) [V,C,V,V,C] 
False
*Assignment03> generates (requireVs 3) [V,C,V,V,C] 
True-}

requireVs :: Int -> Automaton SegmentCV
requireVs n =
    let states = [0 .. n] in
    let syms = [C,V] in
    let i = [0] in
    let f = [n] in
    let ctransitions = map (\q -> (q, C, q)) states in
    let vtransitions = map (\q -> (q, V, q+1)) states in
    (states, syms, i, f, ctransitions ++ vtransitions)