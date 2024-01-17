module Assignment02 where

-- Imports everything from the Recursion module. 
import Recursion

-- Imports just a few things that we have seen from the standard Prelude module. 
-- (If there is no explicit 'import Prelude' line, then the entire Prelude 
-- module is imported. I'm restricting things here to a very bare-bones system.)
import Prelude((+), (-), (*), (<), (>), (++), not, (||), (&&), Bool(..), Int, Show, Char, Eq)

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

-- Write a function mult :: Numb -> (Numb -> Numb) which computes the product of two
-- numbers. You should use the existing add function here, and follow a similar pattern to how
-- we wrote that function. When it’s working, here’s what you should be able to see in ghci:

-- *Assignment02> mult two three
-- S (S (S (S (S (S Z)))))
-- *Assignment02> mult one five
-- S (S (S (S (S Z))))
-- *Assignment02> mult two two
-- S (S (S (S Z)))
-- *Assignment02> mult two (add one two)
-- S (S (S (S (S (S Z)))))

mult :: Numb -> (Numb -> Numb)
mult = \n -> \m -> case n of 
             Z -> Z
             S n' -> add m (mult n' m)

-- Write a function sumUpTo :: Numb -> Numb which computes the sum of all the numbers
-- less than or equal to the given number. For example, given (our representation of) 4, the result
-- should be (our representation of) 10, since 0 + 1 + 2 + 3 + 4 = 10.
-- *Assignment02> sumUpTo four
-- S (S (S (S (S (S (S (S (S (S Z)))))))))
-- *Assignment02> sumUpTo two
-- S (S (S Z))
-- *Assignment02> sumUpTo Z
-- Z

sumUpTo :: Numb -> Numb
sumUpTo = \n -> case n of
            Z -> Z
            S n' -> add (S n') (sumUpTo n')

{-
C. Write a function isEqual :: Numb -> (Numb -> Bool) which returns True if the two
numbers given are equal, and False otherwise. For the purpose of this exercise, do this without
using Haskell’s built-in == function. (Hint: For this one you need to work recursively on both
arguments, like the way bigger works recursively on both of its Numb arguments.)
*Assignment02> isEqual two three
False
1
LING185A, Fall 2023 Laurel Perkins
*Assignment02> isEqual three three
True
*Assignment02> isEqual (sumUpTo three) (S five)
True
*Assignment02> isEqual (sumUpTo four) (add five five)
True
-}

isEqual :: Numb -> (Numb -> Bool)
isEqual = \n -> \m -> case n of 
                      Z -> case m of 
                            Z -> True
                            S m' -> False
                      S n' -> case m of 
                            Z -> False
                            S m' -> isEqual n' m'

{-}
D. Write a function numbToInt :: Numb -> Int which converts a number represented in
our Numb type into Haskell’s built-in integer type Int.
*Assignment02> numbToInt (S (S (S Z)))
3
*Assignment02> numbToInt (double four)
8
*Assignment02> numbToInt Z
0
*Assignment02> (numbToInt three) + (numbToInt one)
4
-}

numbToInt :: Numb -> Int
numbToInt = \n -> case n of
                  Z -> 0
                  S n' -> 1 + numbToInt n'

{-
E. Write a function count :: (a -> Bool) -> ([a] -> Numb) which returns (in the form
of a Numb) the number of elements in the given list for which the given argument returns True.
(Notice that this is a bit like the contains function.)
*Assignment02> count (\x -> x > 3) [2,5,8,11,14]
S (S (S (S Z)))
*Assignment02> count (\x -> x < 10) [2,5,8,11,14]
S (S (S Z))
*Assignment02> count isOdd [two, three, four]
S Z
*Assignment02> count (\x -> x) [True, False, True, True]
S (S (S Z))
*Assignment02> count denotation [f1, Neg f1]
S Z
*Assignment02> count denotation [f1, Neg f1, Dsj f1 (Neg f1)]
S (S Z)
-}

count :: (a -> Bool) -> ([a] -> Numb)
count = \f -> \l -> case l of 
                    [] -> Z
                    x : rest -> case (f x) of
                                True -> S (count f rest)
                                False -> count f rest

{-
F. Write a function addToEnd :: a -> ([a] -> [a]) such that addToEnd x l returns a
list which is like l but has an additional occurrence of x at the end.
*Assignment02> addToEnd 2 [5,5,5,5]
[5,5,5,5,2]
*Assignment02> addToEnd 3 []
[3]
*Assignment02> addToEnd True [False, False, True]
[False,False,True,True]
2
-}

addToEnd :: a -> ([a] -> [a])
addToEnd = \x -> \l -> case l of
                        [] -> [x]
                        y : rest -> y : addToEnd x rest

{- G. Write a function remove :: (a -> Bool) -> ([a] -> [a]) such that remove f l
returns a list which is like l but with those elements for which f returns True removed. (Hint:
A common mistake here is to think about the task as changing the input list into a new list.
But that’s not what needs happen at all. The task is to construct a new list in a way that
depends on, or is “guided by”, the contents of the input list.)
*Assignment02> remove (\x -> x > 3) [2,5,8,11,14]
[2]
*Assignment02> remove (\x -> x < 10) [2,5,8,11,14]
[11,14]
*Assignment02> remove isOdd [two, three, four]
[S (S Z),S (S (S (S Z)))]
*Assignment02> remove isOdd [two, three, five]
[S (S Z)]
*Assignment02> remove (\x -> x) [True, False, True, True]
[False] -}

remove :: (a -> Bool) -> ([a] -> [a])
remove = \f -> \l -> case l of 
                     [] -> []
                     x : rest -> case (f x) of 
                                 True -> remove f rest
                                 False -> x : remove f rest

{- H. Write a function prefix :: Numb -> ([a] -> [a]), such that prefix n list returns
the list containing the first n elements of list; or, if n is greater than the length of list,
returns list as it is. (Hint: For this one you’ll also need to work recursively on two arguments,
like the way bigger does.)
*Assignment02> prefix two [Rock,Paper,Scissors]
[Rock,Paper]
*Assignment02> prefix three [Rock,Paper,Scissors]
[Rock,Paper,Scissors]
*Assignment02> prefix four [Rock,Paper,Scissors]
[Rock,Paper,Scissors]
*Assignment02> prefix Z [Rock,Paper,Scissors]
[] 
-}

prefix :: Numb -> ([a] -> [a])
prefix = \n -> \l -> case n of 
                     Z -> []
                     S n' -> case l of 
                             [] -> []
                             x : rest -> x : prefix n' rest

{- I. Write a recursive function reverse :: [a] -> [a], which returns the reverse of the given
list. One of the other list functions you wrote in this section will be helpful to use here. (See
the third test case below for a hint.)
*Assignment02> reverse [1,2,3]
[3,2,1]
*Assignment02> reverse [True, False, True, True]
[True, True, False, True]
*Assignment02> reverse (5 : [6,7,8])
[8,7,6,5]   
-}

reverse :: [a] -> [a]
reverse = \l -> case l of 
                [] -> []
                x : rest -> addToEnd x (reverse rest)

{- J. Write a function countStars :: RegExp a -> Numb which returns the number of occurrences
of the star operator in the given regular expression. The add function is helpful here. (The
regular expressions re_27a and re_27c are defined in Recursion.hs).
*Assignment02> re_27a
Alt (Lit 'a') (Lit 'b')
*Assignment02> re_27c
Star (Concat (Alt (Lit 'a') (Lit 'b')) (Lit 'c'))
*Assignment02> countStars re_27a
Z
*Assignment02> countStars re_27c
S Z
*Assignment02> countStars (Concat (Star re_27a) (Star re_27c))
S (S (S Z))
*Assignment02> countStars (Star (Star (Star (Star (Star (Lit 'a'))))))
S (S (S (S (S Z)))) -}

countStars :: RegExp a -> Numb
countStars = \a -> case a of
                    Lit x -> Z
                    Alt r1 r2 -> add (countStars r1) (countStars r2)
                    Concat r1 r2 -> add (countStars r1) (countStars r2)
                    Star r -> S (countStars r)
                    ZeroRE -> Z
                    OneRE -> Z

{- K. We can represent the structure of a RegExp with a tree, as shown below for re_27a and
re_27c. Write a function depth :: RegExp a -> Numb which returns the length of the
longest root-to-leaf sequence of nodes in this tree for the given regular expression, i.e. the
depth of the most deeply-embedded leaf of the tree. In a one-node tree, this is one. (Notice
that there is no separate Lit node on top of a character; there is exactly one node in the tree
for each RegExp, and Lit ’a’ is a RegExp but ’a’ is not.) The bigger function is useful here.
*Assignment02> depth re_27a
S (S Z)
*Assignment02> depth re_27c
S (S (S (S Z)))
*Assignment02> depth (Concat (Star re_27a) (Star re_27c))
S (S (S (S (S (S Z)))))
*Assignment02> depth (Star (Star (Star (Star (Star (Lit 'a'))))))
S (S (S (S (S (S Z)))))
*Assignment02> depth ZeroRE
S Z -}

depth :: RegExp a -> Numb
depth = \a -> case a of 
                Lit x -> S Z
                Alt r1 r2 -> case (depth r1, depth r2) of 
                                (d1, Z) -> S d1
                                (Z, d2) -> S d2
                                (S d1, S d2) -> S(S(bigger d1 d2))
                Concat r1 r2 -> case (depth r1, depth r2) of
                                    (d1, Z) -> S d1
                                    (Z, d2) -> S d2
                                    (S d1, S d2) -> S(S(bigger d1 d2))
                Star r1 -> S (depth r1)
                ZeroRE -> S Z
                OneRE -> S Z

{- L. Write a function reToString :: RegExp Char -> [Char] which returns the string—
i.e. the list of characters— representing the given regular expression, assuming that regular
expression is using type Char. Use the notation we used in class, with some obvious simplifications:
use a plain asterisk in place of the superscript asterisk, and use a period in place of the
“center dot”. Don’t forget to include all parentheses. You can use the ++ operator here to
concatenate strings. (There is no distinction between strings and lists of characters: "abc" is
just a convenient shorthand for [’a’,’b’,’c’]).
*Assignment02> "abc" ++ "def"
"abcdef"
*Assignment02> reToString re_27a
"(a|b)"
*Assignment02> reToString re_27b
"((a|b).c)"
*Assignment02> reToString re_27c
"((a|b).c)*"
*Assignment02> reToString (Concat (Star re_27a) (Star re_27c))
"((a|b)*.((a|b).c)**)"
*Assignment02> reToString (Star (Star (Star (Star (Star (Lit 'a'))))))
"a*****"
*Assignment02> reToString (Concat ZeroRE OneRE)
"(0.1)" -}

reToString :: RegExp Char -> [Char]
reToString = \r -> case r of 
                    Lit x -> [x]
                    Alt r1 r2 -> "(" ++ reToString r1 ++ "|" ++ reToString r2 ++ ")"
                    Concat r1 r2 -> "(" ++ reToString r1 ++ "." ++ reToString r2 ++ ")"
                    Star r1 -> reToString r1 ++ "*"
                    ZeroRE -> "0"
                    OneRE -> "1"