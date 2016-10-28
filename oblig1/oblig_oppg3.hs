import Data.Char

data Ast = Number Integer | Name String | App Ast [Ast] | Block [Ast]
	deriving (Eq,Show,Ord)

tokenize :: String -> [String]
addAst :: Ast -> Ast -> Ast
parseExpr :: [String] -> (Ast, [String])
parseApp :: [String] -> (Ast, [String])
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
		else if x == ' ' || x == ','
			then tokenize(xs)
			else [x] : tokenize xs

-- PARSE

addAst (App name list) ast = App name (ast : list)

parseExpr ("(":s) = parseApp s
parseExpr (x:xs)
	| all isDigit x = (Number (read x), xs)
	| otherwise = error "Illegal input"

parseApp (x:xs)
	| x == ")" = (App (Name (head xs)) [], tail xs)
	| otherwise = (addAst (fst app) (fst expr), (snd app))
		where
			expr = parseExpr (x:xs)
			app = parseApp (snd expr)

parseBlock (x:xs) = (Block [(fst expr)], snd expr)
	where expr = parseExpr (x:xs)

parse str = fst (parseBlock (tokenize str))

-- EVAL

calc name (Number x) (Number y)
	| name == "+" = Number (x + y)
	| name == "-" = Number (x - y)
	| name == "*" = Number (x * y)
	| name == "/" = Number (div x y)
	| otherwise = error ("Bad operator: " ++ name)

eval (Number x) = Number x
eval (App (Name name) asts) = calc name (eval (head asts)) (eval (last asts))
eval (Block asts) = eval (head asts)

run str = eval (parse str)