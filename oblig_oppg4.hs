module Oblig12016 where
import Data.Char

data Ast = Number Integer | Name String | App Ast [Ast] | Block [Ast] | Case Ast [Ast] | Bool Ast Ast Ast | Default
	deriving (Eq,Show,Ord)

tokenize :: String -> [String]
addAst :: Ast -> Ast -> Ast
parseExpr :: [String] -> (Ast, [String])
parseApp :: [String] -> (Ast, [String])
parseCase :: [String] -> (Ast, [String])
parseBool :: [String] -> (Ast, [String])
parseBlock :: [String] -> (Ast, [String])
parse :: String -> Ast
calc :: String -> Ast -> Ast -> Ast
eval :: Ast -> Ast
run :: String -> Ast

-- TOKENIZE

tokenize [] = []
tokenize (x:xs) =
	if isDigit x
		then (x : takeWhile isDigit xs) : tokenize (dropWhile isDigit xs)
		else if isAlpha x
			then (x : takeWhile isAlpha xs) : tokenize (dropWhile isAlpha xs)
			else if x == ' ' || x == ',' || x == '.'
				then tokenize(xs)
				else [x] : tokenize xs

-- PARSE

addAst (App name list) ast = App name (ast : list)

parseExpr ("(":s) = parseApp s
parseExpr ("case":s) = parseCase ("case":s)
parseExpr (x:xs)
	| all isDigit x = (Number (read x), xs)
	| otherwise = error ("Illegal input: " ++ x)

parseApp (x:xs)
	| x == ")" = (App (Name (head xs)) [], tail xs)
	| otherwise = (addAst (fst app) (fst expr), (snd app))
		where
			expr = parseExpr (x:xs)
			app = parseApp (snd expr)

parseCase (x:xs)
	| x == "case" =
		if (head xs) == "otherwise"
			then (Case Default [fst defaultExpr], snd defaultExpr)
			else (Case (fst bool) ((fst expr) : [fst (parseCase (snd expr))]), snd (parseCase (snd expr)))
	| otherwise = error "Bad case"
		where
			bool = parseBool xs
			expr = parseExpr (drop 2 (snd bool))
			defaultExpr = parseExpr (drop 3 xs)

parseBool ("(":s) =
	if firstSymbol == "=" || firstSymbol == "!"
		then (Bool (Name (firstSymbol ++ secondSymbol)) (fst firstExpr) (fst secondExpr), (drop 3 (snd secondExpr)))
		else (Bool (Name firstSymbol) (fst firstExpr) (fst secondExpr), (drop 2 (snd secondExpr)))
	where
		firstExpr = parseExpr s
		secondExpr = parseExpr (snd firstExpr)
		firstSymbol = (snd secondExpr) !! 1
		secondSymbol = (snd secondExpr) !! 2

parseBlock (x:xs) = (Block [(fst expr)], snd expr)
	where expr = parseExpr (x:xs)

parse str = fst (parseBlock (tokenize str))

-- EVAL

calc name (Number x) (Number y)
	| name == "+" = Number (x + y)
	| name == "-" = Number (x - y)
	| name == "*" = Number (x * y)
	| name == "/" = Number (div x y)
	| name == "==" = if x == y then Number 1 else Number 0
	| name == "!=" = if x /= y then Number 1 else Number 0
	| name == "<" = if x < y then Number 1 else Number 0
	| name == ">" = if x > y then Number 1 else Number 0
	| otherwise = error ("Bad operator: " ++ name)

eval (Number x) = Number x
eval (App (Name name) asts) = calc name (eval (head asts)) (eval (last asts))
eval (Default) = Number 1
eval (Bool (Name name) expr1 expr2) = calc name (eval expr1) (eval expr2)
eval (Case bool exprs) = if (eval bool) == (Number 1) then eval (head exprs) else eval (last exprs)
eval (Block asts) = eval (head asts)

run str = eval (parse str)