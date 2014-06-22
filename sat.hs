module SAT where

import Data.Maybe

data Literal = P Int | N Int deriving (Show, Eq)
type Clause = [Literal]
type CNF = [Clause]

simplifyClause :: Clause -> Int -> Bool -> Maybe Clause
simplifyClause [] _ _ = Just []
simplifyClause (x:xs) y b | extract x /= y = simplifyClause xs y b >>= (\k -> Just (x:k))
simplifyClause (P _:_) _ True = Nothing
simplifyClause (N _:_) _ False = Nothing
simplifyClause (_:xs) y b = simplifyClause xs y b

simplifyFormula :: CNF -> Int -> Bool -> CNF
simplifyFormula cnf x b = mapMaybe (\k -> simplifyClause k x b) cnf

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
purge f l = filter (notElem l) f

isPure :: CNF -> Literal -> Bool
isPure f (P x) = N x `notElem` concat f
isPure f (N x) = P x `notElem` concat f

chooseLiteral :: CNF -> Int
chooseLiteral f | null (concat f) = 0
chooseLiteral f = extract (head (concat f))

extract :: Literal -> Int
extract (P x) = x
extract (N x) = x

dpll :: CNF -> Bool
dpll [] = True
dpll x | any null x = False
       | null x' = True
       | any null x' = False
       | otherwise   = dpll ([P l] : x') || dpll ([N l] : x')
  where
    l  = chooseLiteral x'
    x' = foldl pureLiteralAssign up up 
    up = foldl unitPropagate x x 
        
