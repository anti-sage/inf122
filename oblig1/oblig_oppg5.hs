module Oblig12016 where
import Data.Char
import Data.Maybe

data Ast = Number Integer | Name String | App Ast [Ast] | Block [Ast] | Case Ast [Ast] | Bool Ast Ast Ast | Default | Set String Ast
	deriving (Eq,Show,Ord)

type Memory = (Integer, Integer -> Maybe Ast)
newtype Context = Context (String -> Maybe Integer)
instance Show Context where
	show _ = ""

tokenize :: String -> [String]
addAst :: Ast -> Ast -> Ast
parseExpr :: [String] -> (Ast, [String])
parseApp :: [String] -> (Ast, [String])
parseCase :: [String] -> (Ast, [String])
parseBool :: [String] -> (Ast, [String])
parseSet :: [String] -> (Ast, [String])
parseBlock :: [String] -> (Ast, [String])
parse :: String -> Ast
emptyMem :: Memory
addToMem :: Memory -> Ast -> (Integer, Memory)
lookupMem :: Memory -> Integer -> Maybe Ast
emptyCtx :: Context
addToCtx :: Context -> String -> Integer -> Context
lookupCtx :: Context -> String -> Maybe Integer
calc :: String -> Ast -> Ast -> Ast
eval :: Ast -> Context -> Memory -> (Ast, Context, Memory)
run :: String -> Ast

-- TOKENIZE

tokenize [] = []
tokenize (x:xs) =
	if isDigit x
		then (x : takeWhile isDigit xs) : tokenize (dropWhile isDigit xs)
		else if isAlpha x
			then (x : takeWhile isAlpha xs) : tokenize (dropWhile isAlpha xs)
			else if x == ' ' || x == '.'
				then tokenize(xs)
				else [x] : tokenize xs

-- PARSE

addAst (App name list) ast = App name (ast : list)
addAst (Block list) ast = Block (ast : list)

parseExpr ("(":s) = parseApp s
parseExpr ("case":s) = parseCase ("case":s)
parseExpr ("set":s) = parseSet s
parseExpr (x:xs)
	| all isDigit x = (Number (read x), xs)
	| all isAlpha x = (Name x, xs)
	| otherwise = error ("Illegal input: " ++ x)

parseApp (",":xs) = parseApp xs
parseApp (x:xs)
	| x == ")" = (App (Name (head xs)) [], tail xs)
	| otherwise = (addAst (fst app) (fst expr), (snd app))
		where
			expr = parseExpr (x:xs)
			app = parseApp (snd expr)

parseCase (",":xs) = parseCase xs
parseCase (x:xs)
	| x == "case" =
		if (head xs) == "otherwise"
			then (Case Default [fst defaultExpr], snd defaultExpr)
			else (Case (fst bool) ((fst expr) : [fst next]), snd next)
	| otherwise = error "Bad case"
		where
			bool = parseBool xs
			expr = parseExpr (drop 2 (snd bool))
			defaultExpr = parseExpr (drop 3 xs)
			next = parseCase (snd expr)

parseBool ("(":s) =
	if firstSymbol == "=" || firstSymbol == "!"
		then (Bool (Name (firstSymbol ++ secondSymbol)) (fst firstExpr) (fst secondExpr), (drop 3 (snd secondExpr)))
		else (Bool (Name firstSymbol) (fst firstExpr) (fst secondExpr), (drop 2 (snd secondExpr)))
	where
		firstExpr = parseExpr s
		secondExpr = parseExpr (drop 1 (snd firstExpr))
		firstSymbol = (snd secondExpr) !! 1
		secondSymbol = (snd secondExpr) !! 2

parseSet (x:xs) = (Set x (fst expr), snd expr)
	where expr = parseExpr xs

parseBlock [] = (Block [], [])
parseBlock (x:xs) = (addAst (fst block) (fst expr), snd block)
	where
		expr = parseExpr (x:xs)
		block = parseBlock (drop 1 (snd expr))

parse str = fst (parseBlock (tokenize str))

-- EVAL

emptyMem = (0, \ _ -> Nothing)
addToMem mem ast = (fst mem, ((fst mem) + 1, (\ x -> if x == (fst mem) then Just ast else (snd mem) x)))
lookupMem mem i = (snd mem) i

emptyCtx = Context (const Nothing)
addToCtx (Context ctx) name i = Context (\ n -> if n == name then Just i else ctx n)
lookupCtx (Context ctx) name = ctx name

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

eval (Number x) c m = (Number x, c, m)

eval (App (Name name) asts) c m = (calc name expr1 expr2, c3, m3)
	where
		(expr1, c2, m2) = eval (head asts) c m
		(expr2, c3, m3) = eval (last asts) c2 m2

eval (Default) c m = (Number 1, c, m)

eval (Bool (Name name) e1 e2) c m = (calc name expr1 expr2, c3, m3)
	where
		(expr1, c2, m2) = eval e1 c m
		(expr2, c3, m3) = eval e2 c2 m2

eval (Case bool exprs) c m = if expr1 == (Number 1) then (expr2, c3, m3) else (expr3, c4, m4)
	where
		(expr1, c2, m2) = eval bool c m
		(expr2, c3, m3) = eval (head exprs) c2 m2
		(expr3, c4, m4) = eval (last exprs) c2 m2

eval (Set name expr) c m = (val, addToCtx c name i, m3)
	where
		(val, c2, m2) = eval expr c m
		(i, m3) = addToMem m2 val

eval (Name name) c m =
	if index == Nothing
		then error ("Variable \"" ++ name ++ "\" not set")
		else (fromJust (lookupMem m (fromJust index)), c, m)
	where
		index = lookupCtx c name

eval (Block [x]) c m = eval x c m
eval (Block (x:xs)) c m = (eval (Block xs) c2 m2)
	where
		(expr, c2, m2) = eval x c m

run str = expr where (expr, c, m) = eval (parse str) emptyCtx emptyMem