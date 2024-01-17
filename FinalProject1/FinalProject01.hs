{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Use list literal pattern" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use :" #-}
module FinalProject01 where

import Control.Applicative(liftA, liftA2, liftA3)
import Data.List

import CFGParsing

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- These functions are placeholders to work with 'bottomUp' in Part 1.3. 
-- You should replace 'undefined' in these functions with your own code.

--1.1   

  --A

-- Description: Checks if a given rewrite rule is in Chomsky Normal Form (CNF).
isRuleCNF :: RewriteRule nt t -> Bool
isRuleCNF rule = case rule of
  (NTRule _ []) -> False  -- Empty right-hand side for NTRule is not allowed
  (NTRule _ (x:xs)) -> length (x:xs) == 2  -- Accepts NTRule if it has exactly two elements on the right-hand side.
  (TRule _ _) -> True -- checks if the rule is a terminal rule (TRule)
  NoRule -> True

  --B

-- Description: Determines if an entire context-free grammar (CFG) is in Chomsky Normal Form.
isCNF :: CFG nt t -> Bool
isCNF (_, _, _, rules) = all isRuleCNF rules


--1.2

-- Description: Finds all paths to a goal state in an FSA.
-- Parameters:
--    current - the current state and symbols.
--    history - history of states and symbols visited.
--    rules - a list of transition rules for the FSA.
--    goals - a list of goal states.
-- Variables:
--   validNextSteps - potential next steps based on current state and rules.
--   exploreNextSteps - function to recursively explore next steps in the FSA.
pathsToGoalFSA :: (Eq st, Eq sy) =>
                      ((st,[sy]), [(st,[sy])]) -> [(st,sy,st)] ->
                      [(st,[sy])] -> [[(st,[sy])]]
pathsToGoalFSA (current, history) rules goals =
        case elem current goals of
        True -> [history ++ [current]] -- if at goal state, append to history and return list
        False ->  case consumeFSA rules current of
                    [] -> []
                    _ -> concatMap exploreNextSteps validNextSteps
        where
            validNextSteps = consumeFSA rules current
            exploreNextSteps nextStep =
                pathsToGoalFSA (nextStep, history ++ [current]) rules goals


-- pathsToGoalFSA ((2, [VW,C,VW]),[(1,[C,VW,C,VW])]) (getDelta fsa13) (getGoalConfigs fsa13)
-- [[(1,[C,VW,C,VW]),(2,[VW,C,VW]),(1,[C,VW]),(2,[VW]),(1,[])], [(1,[C,VW,C,VW]),(2,[VW,C,VW]),(3,[C,VW]),(1,[VW]),(1,[])]]

-- pathsToGoalFSA ((1,[VW,VW]),[(1,[C,VW,VW,VW]),(2,[VW,VW,VW])]) (getDelta fsa13) (getGoalConfigs fsa13)
-- [[(1,[C,VW,VW,VW]),(2,[VW,VW,VW]),(1,[VW,VW]),(1,[VW]),(1,[])]] 

-- pathsToGoalFSA ((3,[VW,VW]),[(1,[C,VW,VW,VW]),(2,[VW,VW,VW])]) (getDelta fsa13) (getGoalConfigs fsa13)
-- []



--C

-- Description: Performs the 'shift' action in parsing, moving the input symbol to the stack.
-- Parameters:
--    rules - a list of rewrite rules.
--    nt:rest2 - stack configuration.
--    x:rest - remaining input symbols.
-- Variables:
--   matchingRules - list of rules whose rhs matches the current terminal symbol from the input.
shift :: (Eq nt, Eq t) => [RewriteRule nt t] -> Config nt t -> [ParseStep nt t]
shift rules (nt:rest2, x:rest) =
    let matchingRules = filter (\rule -> case rule of TRule _ _ -> rhsTRule rule == x; _ -> False) rules
    in
      case matchingRules of
        [] -> []
        x' : [] -> [ParseStep Shift x' (nt:rest2 ++ [NoBar(lhs x')], rest)] -- add shifted non-terminal to front of stack
        x' : rest' -> ParseStep Shift x' (nt:rest2 ++ [NoBar(lhs x')], rest) : shift rest' (nt:rest2, x:rest)

shift rules ([], x:rest) =
    let matchingRules = filter (\rule -> case rule of TRule _ _ -> rhsTRule rule == x; _ -> False) rules
    in
      case matchingRules of
        [] -> []
        x' : [] -> [ParseStep Shift x' ([NoBar(lhs x')], rest)]
        x' : rest' -> ParseStep Shift x' ([NoBar(lhs x')], rest) : shift rest' ([], x:rest)

shift rules ([],[]) = []
shift rules (nt:rest2, []) = [] -- cannot shift if no words in input list


-- Description: Extracts the non-terminal from a stack element.
-- Parameters:
--    nt - a stack element (either Bar or NoBar).
extractNt :: Stack nt -> nt
extractNt (NoBar nt) = nt
extractNt (Bar nt) = nt

-- Description: Extracts non-terminals from a list of stack elements.
-- Parameters:
--    stackList - a list of stack elements.
extractNts :: [Stack nt] -> [nt]
extractNts stackList = map extractNt stackList 

-- Description: Wraps a non-terminal in a NoBar stack element.
makeStack :: nt -> Stack nt 
makeStack nt = (NoBar nt)

-- Description: Takes elements from the end of a list in a right-to-left idiomatic manner.
-- Parameters:
--   n - number of elements to take from the specified list.
--   l - the list from which elements are taken.
takeRIdiomatic :: Int -> [Stack nt] -> [Stack nt]
takeRIdiomatic n l = go (drop n l) l
  where
    go [] r = r
    go (_:xs) (_:ys) = go xs ys

-- Description: Drops the last 'n' elements from a list.
-- Parameters:
--   n - number of elements to drop from the end of the list.
--   xs - the list to be processed.
dropLastN :: Int -> [Stack nt] -> [Stack nt]
dropLastN n xs = take (length xs - n) xs


-- Description: Executes a 'reduce' operation in a parser, reducing a sequence of symbols to a non-terminal.
-- Parameters:
--   rules - a list of grammar rewrite rules.
--   (nt:rest2, x:rest) - a tuple of current stack (nt:rest2) and remaining input symbols (x:rest).
-- Variables:
--   extractedLists - lists of every possible number of non-terminals extracted from the stack, to find matching rules with.
--   matchingRules - rules that match with the extracted non-terminals.
--   dropCount - number of elements to drop from the stack during reduction, based on the number of non-terminals being replaced by the rule.
reduce :: (Eq nt, Eq t) => [RewriteRule nt t] -> Config nt t -> [ParseStep nt t]
reduce rules ([],[]) = [] 
reduce rules ([], x:rest) = [] -- cannot reduce if no non-terminals on stack

reduce rules (nt:rest2, x:rest) =
    let extractedLists = [takeRIdiomatic i (nt:rest2) | i <- [1..length (nt:rest2)]] -- list of different amounts of nts from nt:rest2 (1, 2, 3, etc.)
        matchingRules = findMatchingRules extractedLists (nt:rest2) rules
    in
    case matchingRules of
        [] -> []
        rule2 : [] -> 
            let dropCount = length (rhsNTRule rule2) in 
                [ParseStep Reduce rule2 ((dropLastN dropCount (nt:rest2)) ++ [makeStack (lhs rule2)], x:rest)] -- replace non-terminals on stack with corresponding non-terminal from lhs of rule
        rule2 : rest' ->
            let dropCount = length (rhsNTRule rule2) in 
                ParseStep Reduce rule2 ((dropLastN dropCount (nt:rest2)) ++ [makeStack (lhs rule2)], x:rest) : reduce rest' (nt:rest2, x:rest)
-- case where there are no terminals in the input
reduce rules (nt:rest2, []) =
    let extractedLists = [takeRIdiomatic i (nt:rest2) | i <- [1..length (nt:rest2)]] -- list of different amounts of nts from nt:rest2 (1, 2, 3, etc.)
        matchingRules = findMatchingRules extractedLists (nt:rest2) rules
    in
    case matchingRules of
        [] -> []
        rule2 : [] -> 
            let dropCount = length (rhsNTRule rule2) in 
                [ParseStep Reduce rule2 ((dropLastN dropCount (nt:rest2)) ++ [makeStack (lhs rule2)], [])]
        rule2 : rest' ->
            let dropCount = length (rhsNTRule rule2) in 
                ParseStep Reduce rule2 ((dropLastN dropCount (nt:rest2)) ++ [makeStack (lhs rule2)], []) : reduce rest' (nt:rest2, [])


-- Description: Identifies matching grammar rules for a given list of non-terminals, to use in reduction.
-- Parameters:
--   nts - a list of non-terminals to match.
--   allNTs - all non-terminals available in the context.
--   rules - a list of grammar rewrite rules.
findMatchingRules :: (Eq nt, Eq t) => [[Stack nt]] -> [Stack nt] -> [RewriteRule nt t] -> [RewriteRule nt t]
findMatchingRules [] _ _ = [] 
findMatchingRules (nts:restNTs) allNTs rules =
    case find (\rule -> case rule of NTRule _ _ -> rhsNTRule rule == extractNts nts; _ -> False) rules of
        Just matchingRule -> matchingRule : findMatchingRules restNTs allNTs rules -- if rule's rhs matches rules in grammar, add to list
        Nothing -> findMatchingRules restNTs allNTs rules -- if rule's rhs does not match any rules in grammar, proceed to next rule


--D

-- Description: Implements a bottom-up parsing strategy using a given CFG and input, with appropriate start/goal configs to feed into the parser.
-- Variables:
--   startingConfig - the initial configuration of the parser.
--   goalConfig - the desired goal configuration for successful parsing.
bottomUp :: (Eq nt, Eq t) => CFG nt t -> [t] -> [[ParseStep nt t]]
bottomUp cfg input =
  let (nts, ts, start, rules) = cfg in
  let startingConfig = ([], input) in
  let goalConfig = ([NoBar start], []) in
  parser [shift, reduce] rules startingConfig goalConfig


-- Description: Checks if the parser has exceeded a certain length, to prevent overflow or extreme slowness on some left corner parses.
-- Parameters:
--   (stack, _) - the current configuration of the parser, with its 'stack' being the main focus.
notMakingProgress :: Config nt t -> Bool
notMakingProgress (stack, _) = length (stack) > 8


-- Description: Orchestrates the parsing process using various parsing strategies, based on start/goal configs of the type of parse to be done.
-- Variables:
--   findAllParses - a recursive helper function that explores all possible parse paths starting from the current configuration.
--       currentConfig - the configuration at the current step of the parse.
--       currentPath - the list of parse steps taken to reach the current configuration.
--   applyStep - a function that applies a given parsing step function to the current configuration.
--       stepFunc - a single step function from the list of transitionSteps, used to generate the next set of parse steps.
--       config - the current configuration to which the stepFunc is applied.
--       path - the current parse path to which new steps will be appended.
--   processNextStep - a function that processes the next parse step and updates the configuration accordingly.
--       nextStep - a single parse step generated by applyStep, representing one possible next move in the parse process.
--       oldConfig - the configuration before applying nextStep.
--       path - the current parse path to which the nextStep will be appended.
--   newConfig - the updated configuration after applying the nextStep.
parser :: (Eq nt, Eq t)
       => [[RewriteRule nt t] -> Config nt t -> [ParseStep nt t]]
          -- ^ List of transition steps. ^
       -> [RewriteRule nt t]  -- Rules from the CFG.
       -> Config nt t         -- Starting configuration.
       -> Config nt t         -- Goal configuration.
       -> [[ParseStep nt t]]  -- List of possible parses.
parser transitionSteps rules startConfig goalConfig =
    findAllParses startConfig [ParseStep NoTransition NoRule startConfig]
  where
    findAllParses currentConfig currentPath
        | currentConfig == goalConfig = [currentPath]
        | notMakingProgress currentConfig = [] -- uses helper function to terminate current parsing path if stack is getting too long
        | otherwise = concatMap (\stepFunc -> applyStep stepFunc currentConfig currentPath) transitionSteps

    applyStep stepFunc config path =
        let nextSteps = stepFunc rules config  -- stepFunc returns [ParseStep nt t] (list of possible next parse steps)
        in concatMap (\nextStep -> processNextStep nextStep config path) nextSteps

    processNextStep nextStep oldConfig path =
        findAllParses newConfig (path ++ [nextStep])
      where
        newConfig = getConfig nextStep


-- E

-- Description: Converts a list of non-terminals into a list of stack elements wrapped in NoBar.
makeStackList :: [nt] -> [Stack nt]
makeStackList list = map makeStack list


-- Description: Implements the 'predict' step in a top-down parser by adding predicted non-terminals to the stack.
-- Variables:
--   matchingRules - a list of rules that match the current non-terminal on the stack.
predict :: (Eq nt, Eq t) => [RewriteRule nt t] -> Config nt t -> [ParseStep nt t]
predict rules ([],[]) = []
predict rules ([],x:rest) = []
predict rules (nt:rest2, x:rest) =
      let matchingRules = filter (\rule -> case rule of NTRule _ _ -> lhs rule == extractNt nt; _ -> False) rules -- find rules whose lhs symbol matches top of stack
      in
        case matchingRules of
          [] -> []
          rule2 : [] -> [ParseStep Predict rule2 (makeStackList(rhsNTRule rule2) ++ rest2, x:rest)] -- add rhs symbols of rule to front of stack
          rule2 : rest' -> ParseStep Predict rule2 (makeStackList(rhsNTRule rule2) ++ rest2, x:rest) : predict rest' (nt:rest2, x:rest)

predict rules (nt:rest2, []) =
      let matchingRules = filter (\rule -> case rule of NTRule _ _ -> lhs rule == extractNt nt; _ -> False) rules
      in
        case matchingRules of
          [] -> []
          rule2 : [] -> [ParseStep Predict rule2 (makeStackList(rhsNTRule rule2) ++ rest2, [])]
          rule2 : rest' -> ParseStep Predict rule2 (makeStackList(rhsNTRule rule2) ++ rest2, []) : predict rest' (nt:rest2, [])


-- Description: Implements the 'match' operation by checking if the current terminal in the input matches the stack top.
-- Variables:
--   matchingRules - a list of terminal rules where the lhs matches the non-terminal on top of the stack and rhs matches the current input symbol.
match :: (Eq nt, Eq t) => [RewriteRule nt t] -> Config nt t -> [ParseStep nt t]
match rules ([],[]) = []
match rules (nt:rest2, []) = []
match rules ([], x:rest) = []
match rules (nt:rest2, x:rest) =
      let matchingRules = filter (\rule -> case rule of TRule _ _ -> (lhs rule == extractNt nt) && (rhsTRule rule == x); _ -> False) rules
      in
        case matchingRules of
          [] -> []
          rule2 : [] -> [ParseStep Match rule2 (rest2, rest)] -- remove symbols from stack and input list 
          rule2 : rest' -> [ParseStep Match rule2 (rest2, rest)]


-- Description: Implements a top-down parsing strategy for a given CFG and input, with corresponding start/goal configs to feed into the parser.
topDown :: (Eq nt, Eq t) => CFG nt t -> [t] -> [[ParseStep nt t]]
topDown cfg input = 
  let (nts, ts, start, rules) = cfg in
  let startingConfig = ([NoBar start], input) in
  let goalConfig = ([], []) in
  parser [predict, match] rules startingConfig goalConfig

-- topDown cfg4 (words "the baby saw the boy")
-- topDown cfg4 (words "the actor the boy met won")
-- topDown cfg4 (words "John met the boy that saw the actor that won the award")

-- F

--shiftLC [(TRule NP "Mary"), (TRule V "saw")] ([Bar VP], ["saw","the","boy"])

-- Description: Executes a 'shift' operation in left-corner parsing, transitioning the current input symbol to the stack.
-- Variables:
--   matchingRules - terminal rules with rhs matching the current input symbol for shifting.
shiftLC :: (Eq nt, Eq t) => [RewriteRule nt t] -> Config nt t -> [ParseStep nt t]
shiftLC rules (nt:rest2, x:rest) =
    let matchingRules = filter (\rule -> case rule of TRule _ _ -> rhsTRule rule == x; _ -> False) rules
    in
      case matchingRules of
        [] -> []
        x' : [] -> [ParseStep Shift x' ([NoBar(lhs x')] ++ nt:rest2, rest)] -- add matched symbol to front of stack
        x' : rest' -> (ParseStep Shift x' ([NoBar(lhs x')] ++ nt:rest2, rest)) : (shiftLC rest' (nt : rest2, x:rest))

shiftLC rules ([], x:rest) =
    let matchingRules = filter (\rule -> case rule of TRule _ _ -> rhsTRule rule == x; _ -> False) rules
    in
      case matchingRules of
        [] -> []
        x' : [] -> [ParseStep Shift x' ([NoBar(lhs x')], rest)]
        x' : rest' -> (ParseStep Shift x' ([NoBar(lhs x')], rest)) : (shiftLC rest' ([], x:rest))

shiftLC rules ([],[]) = []
shiftLC rules (nt:rest2, []) = []


-- matchLC [(TRule NP "Mary"), (TRule N "baby")] ([Bar N, NoBar NP, Bar S], ["baby","saw","the","boy"])

-- Description: Implements the 'match' operation in left-corner parsing, checking if the current input symbol matches with the expected terminal in the rule.
-- Variables:
--   matchingRules - terminal rules where the lhs matches the non-terminal on the top of the stack and rhs matches the current input symbol.
matchLC :: (Eq nt, Eq t) => [RewriteRule nt t] -> Config nt t -> [ParseStep nt t]
matchLC rules ([],[]) = []
matchLC rules (nt:rest2, []) = []
matchLC rules ([], x:rest) = []
matchLC rules (nt:rest2, x:rest) =
      let matchingRules = filter (\rule -> case rule of TRule _ _ -> (Bar(lhs rule) == nt) && (rhsTRule rule == x); _ -> False) rules
      in
        case matchingRules of
          [] -> []
          rule2 : _ -> [ParseStep Match rule2 (rest2, rest)] -- removed matched symbols from stack and input list


-- Description: Wraps a non-terminal in a Bar stack entity, indicating a left-corner prediction.
makeBar :: nt -> Stack nt 
makeBar nt = Bar nt

-- Description: Converts a list of non-terminals into a list of Bar stack entities, indicating a series of left-corner predictions.
makeBarList :: [nt] -> [Stack nt]
makeBarList list = map makeBar list


-- predictLC [(NTRule VP [V, NP]), (NTRule NP [D,N])] ([NoBar D, Bar S], ["baby","saw","the","boy"])
-- predictLC [(NTRule VP [V]), (NTRule NP [D,N])] ([NoBar V, Bar S], ["saw","the","boy"])
-- predictLC [(NTRule VP [V]), (NTRule NP [D,N,ORC])] ([NoBar D, Bar S], ["saw","the","boy"])

-- Description: Implements the 'predict' operation in left-corner parsing by adding predicted non-terminals to the stack.
-- Variables:
--   matchingRules - a list of rules that predict the next non-terminals based on the current stack top.
predictLC :: (Eq nt, Eq t) => [RewriteRule nt t] -> Config nt t -> [ParseStep nt t]
predictLC rules (nt:rest2, x:rest) =
      let matchingRules = filter (\rule -> case rule of NTRule _ _ -> (take 1 (rhsNTRule rule) == [extractNt nt]); _ -> False) rules
      in 
        case matchingRules of
          [] -> []
          rule2 : [] -> case (length (rhsNTRule rule2)) of
                          1 -> [ParseStep Predict rule2 ([NoBar (lhs rule2)] ++ rest2, x:rest)] -- if rhs of rule only has 1 symbol, just add lhs symbol to front of stack
                          _ -> [ParseStep Predict rule2 (makeBarList(drop 1 (rhsNTRule rule2)) ++ [NoBar(lhs rule2)] ++ rest2, x:rest)] -- otherwise, add rest of rhs symbols and then lhs symbol to front of stack
          rule2 : rest' -> case (length(rhsNTRule rule2)) of
                            1 -> (ParseStep Predict rule2 ([NoBar(lhs rule2)] ++ rest2, x:rest)) : (predictLC rest' (nt:rest2, x:rest))
                            _ -> (ParseStep Predict rule2 (makeBarList(drop 1 (rhsNTRule rule2)) ++ [NoBar(lhs rule2)] ++ rest2, x:rest)) : (predictLC rest' (nt:rest2, x:rest))
predictLC rules (nt:rest2,[]) = []
predictLC rules ([],[]) = []
predictLC rules ([],x:rest) = []

-- connectLC [(NTRule S [NP, PP]), (NTRule S [NP,VP])] ([NoBar NP, Bar S, NoBar PP, Bar VP], [])
-- connectLC [(NTRule S [NP]), (NTRule S [NP,VP,P])] ([NoBar NP, Bar S, NoBar PP, Bar VP], [])

-- Description: Performs the 'connect' operation in left-corner parsing, linking predicted non-terminals with the current input.
-- Variables:
--   matchingRules - rules that connect the current non-terminal with the next predicted element.
connectLC :: (Eq nt, Eq t) => [RewriteRule nt t] -> Config nt t -> [ParseStep nt t]
connectLC rules (nt:z:rest2, x:rest) =
      let matchingRules = filter (\rule -> case rule of NTRule _ _ -> (Bar(lhs rule) == z) && ((take 1 (rhsNTRule rule)) == [extractNt nt]); _ -> False) rules 
      -- Rule's first rhs symbol should match first nt on stack, and rule's lhs symbol should match second nt on stack.
      in
        case matchingRules of 
          [] -> []
          rule2 : [] -> case length(rhsNTRule rule2) of
                        1 -> [ParseStep Connect rule2 (rest2, x:rest)] -- don't add anything if rhs of rule has only one symbol
                        _ -> [ParseStep Connect rule2 (makeBarList(drop 1 (rhsNTRule rule2)) ++ rest2, x:rest)] -- add rhs symbols of rule (excluding first one) to front of stack
          rule2 : rest' -> case length(rhsNTRule rule2) of
                        1 -> (ParseStep Connect rule2 (rest2, x:rest)) : (connectLC rest' (nt:z:rest2, x:rest)) 
                        _ -> (ParseStep Connect rule2 (makeBarList(drop 1 (rhsNTRule rule2)) ++ rest2, x:rest)) : (connectLC rest' (nt:z:rest2, x:rest))

connectLC rules (nt:z:rest2, []) =
      let matchingRules = filter (\rule -> case rule of NTRule _ _ -> (Bar(lhs rule) == z) && ((take 1 (rhsNTRule rule)) == [extractNt nt]); _ -> False) rules
      in
        case matchingRules of 
          [] -> []
          rule2 : [] -> case length(rhsNTRule rule2) of
                        1 -> [ParseStep Connect rule2 (rest2, [])] 
                        _ -> [ParseStep Connect rule2 (makeBarList(drop 1 (rhsNTRule rule2)) ++ rest2, [])] 
          rule2 : rest' -> case length(rhsNTRule rule2) of
                        1 -> (ParseStep Connect rule2 (rest2, [])) : (connectLC rest' (nt:z:rest2, []))
                        _ -> (ParseStep Connect rule2 (makeBarList(drop 1 (rhsNTRule rule2)) ++ rest2, [])) : (connectLC rest' (nt:z:rest2, []))

connectLC rules ([],[]) = []
connectLC rules ([], x:rest) = []
connectLC _ _ = []

-- Description: Implements a left-corner parsing strategy for a given CFG and input string, with appropriate start/goal configs to feed into parser.
leftCorner :: (Eq nt, Eq t) => CFG nt t -> [t] -> [[ParseStep nt t]]
leftCorner cfg input =
    let (nts, ts, start, rules) = cfg in
    let startingConfig = ([Bar start], input) in
    let goalConfig = ([],[]) in
    parser [shiftLC, matchLC, connectLC, predictLC] rules startingConfig goalConfig
 
 
 -- leftCorner cfg4 (words "the baby saw the boy")
 -- leftCorner cfg4 (words "the actor the boy met won")
 -- leftCorner cfg4 (words "Mary 's boss 's baby won")
 -- leftCorner cfg4 (words "John met the boy that saw the actor that won the award")
 -- leftCorner cfg12 (words "watches spies with telescopes")
 -- leftCorner cfg4 (words "the actor the boy the baby saw met won")