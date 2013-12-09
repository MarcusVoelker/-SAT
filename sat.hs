module SAT where

import Data.Maybe

data Literal = P Int | N Int deriving (Show, Eq)
type Clause = [Literal]
type CNF = [Clause]

simplifyClause :: Clause -> Int -> Bool -> Maybe Clause
simplifyClause [] _ _ = Just []
simplifyClause ((P x):xs) y b | x /= y = (simplifyClause xs y b) >>= (\k -> Just ((P x) : k))
simplifyClause ((N x):xs) y b | x /= y = (simplifyClause xs y b) >>= (\k -> Just ((N x) : k))
simplifyClause ((P x):xs) _ True = Nothing
simplifyClause ((N x):xs) _ False = Nothing
simplifyClause ((P x):xs) y b = simplifyClause xs y b
simplifyClause ((N x):xs) y b = simplifyClause xs y b

simplifyFormula :: CNF -> Int -> Bool -> CNF
simplifyFormula cnf x b = catMaybes (map (\k -> simplifyClause k x b) cnf)

unitPropagate :: CNF -> Clause -> CNF
unitPropagate x [P l] = simplifyFormula x l True
unitPropagate x [N l] = simplifyFormula x l False
unitPropagate x _ = x

pureLiteralAssign :: CNF -> Clause -> CNF
pureLiteralAssign = foldl pureLiteralAssign'

pureLiteralAssign' :: CNF -> Literal -> CNF
pureLiteralAssign' f l | not (isPure f l) = f
pureLiteralAssign' f l = purge f l

purge :: CNF -> Literal -> CNF
purge f l = filter (\c -> notElem l c) f

isPure :: CNF -> Literal -> Bool
isPure f (P x) = notElem (N x) (concat f)
isPure f (N x) = notElem (P x) (concat f)

chooseLiteral :: CNF -> Int
chooseLiteral f | null (concat f) = 0
chooseLiteral f = extract (head (concat f))

extract :: Literal -> Int
extract (P x) = x
extract (N x) = x

dpll :: CNF -> Bool
dpll [] = True
dpll x | any null x = False
dpll x | null x' = True
  where
    l  = chooseLiteral x'
    x' = foldl pureLiteralAssign up up 
    up = foldl unitPropagate x x 
dpll x | any null x' = False
  where
    l  = chooseLiteral x'
    x' = foldl pureLiteralAssign up up 
    up = foldl unitPropagate x x 
dpll x = dpll ([P l] : x') || dpll ([N l] : x')
  where
    l  = chooseLiteral x'
    x' = foldl pureLiteralAssign up up 
    up = foldl unitPropagate x x 
        
