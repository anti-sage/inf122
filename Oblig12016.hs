module Oblig12016 where
import Data.Char
import Data.Maybe

data Ast = Number Integer | Name String | App Ast [Ast] | Block [Ast] | Case Ast [Ast] | Bool Ast Ast Ast | Default | Set String Ast | Lambda String Ast | Function String Ast Context
    deriving (Eq,Show,Ord)

type Memory = (Integer, Integer -> Maybe Ast)
newtype Context = Context (String -> Maybe Integer)
instance Show Context where
    show _ = "ctx"
instance Eq Context where
    _ == _ = False
instance Ord Context where
    _ <= _ = False

-- TOKENIZE

tokenize :: String -> [String]
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

addAst :: Ast -> Ast -> Ast
addAst (App name list) ast = App name (ast : list)
addAst (Block list) ast = Block (ast : list)

parseExpr :: [String] -> (Ast, [String])
parseExpr ("(":s) = parseApp s
parseExpr ("case":s) = parseCase ("case":s)
parseExpr ("set":s) = parseSet s
parseExpr ("lambda":s) =
    if (snd lambda /= [] && head (snd lambda) == "(")
        then (App (fst lambda) [fst expr], drop 1 (snd expr))
        else lambda
    where
        lambda = parseLambda ("lambda":s)
        expr = parseExpr (drop 1 (snd lambda))
parseExpr (x:xs)
    | all isDigit x = (Number (read x), xs)
    | all isAlpha x =
        if (length xs > 1 && head xs == "(")
            then
                let expr = parseExpr (tail xs)
                    in (App (Name x) [fst expr], (drop 1 (snd expr)))
            else (Name x, xs)
    | otherwise = error ("Illegal input: " ++ x)

parseApp :: [String] -> (Ast, [String])
parseApp (",":xs) = parseApp xs
parseApp (x:xs)
    | x == ")" = (App (Name (head xs)) [], tail xs)
    | otherwise = (addAst (fst app) (fst expr), (snd app))
        where
            expr = parseExpr (x:xs)
            app = parseApp (snd expr)

parseCase :: [String] -> (Ast, [String])
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

parseBool :: [String] -> (Ast, [String])
parseBool ("(":s) =
    if firstSymbol == "=" || firstSymbol == "!"
        then (Bool (Name (firstSymbol ++ secondSymbol)) (fst firstExpr) (fst secondExpr), (drop 3 (snd secondExpr)))
        else (Bool (Name firstSymbol) (fst firstExpr) (fst secondExpr), (drop 2 (snd secondExpr)))
    where
        firstExpr = parseExpr s
        secondExpr = parseExpr (drop 1 (snd firstExpr))
        firstSymbol = (snd secondExpr) !! 1
        secondSymbol = (snd secondExpr) !! 2

parseSet :: [String] -> (Ast, [String])
parseSet (x:xs) = (Set x (fst expr), snd expr)
    where expr = parseExpr xs

parseLambda :: [String] -> (Ast, [String])
parseLambda ("lambda":s) = (Lambda (head s) (fst expr), drop 1 (snd expr))
    where
        expr = parseExpr (drop 2 s)

parseBlock :: [String] -> (Ast, [String])
parseBlock [] = (Block [], [])
parseBlock (x:xs) = (addAst (fst block) (fst expr), snd block)
    where
        expr = parseExpr (x:xs)
        block = parseBlock (drop 1 (snd expr))

parse :: String -> Ast
parse str = fst (parseBlock (tokenize str))

-- EVAL

emptyMem :: Memory
emptyMem = (0, \ _ -> Nothing)

addToMem :: Memory -> Ast -> (Integer, Memory)
addToMem mem ast = (fst mem, ((fst mem) + 1, (\ x -> if x == (fst mem) then Just ast else (snd mem) x)))

updateMem :: Memory -> Integer -> Ast -> (Integer, Memory)
updateMem mem i ast = (i, ((fst mem), (\ x -> if x == i then Just ast else (snd mem) x)))

lookupMem :: Memory -> Integer -> Maybe Ast
lookupMem mem i = (snd mem) i


emptyCtx :: Context
emptyCtx = Context (const Nothing)

addToCtx :: Context -> String -> Integer -> Context
addToCtx (Context ctx) name i =    Context (\ n -> if n == name then Just i else ctx n)

lookupCtx :: Context -> String -> Maybe Integer
lookupCtx (Context ctx) name = ctx name


calc :: String -> Ast -> Ast -> Ast
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

eval :: Ast -> Context -> Memory -> (Ast, Context, Memory)
eval (Number x) c m = (Number x, c, m)

eval (App (Name name) asts) c m
    | name == "+" || name == "-" || name == "*" || name == "/" = (calc name expr1 expr2, c3, m3)
    | otherwise = eval (App func [head asts]) c m
    where
        (expr1, c2, m2) = eval (head asts) c m
        (expr2, c3, m3) = eval (last asts) c2 m2
        (func, _, _) = eval (Name name) c m

eval (App (Lambda var funExpr) expr) c m = eval (App lambda expr) c2 m2
    where
        (val, c2, m2) = eval (head expr) c m
        (lambda, _, _) = eval (Lambda var funExpr) c m

eval (App (Function var funExpr context) [(Name expr)]) c m = (res, c2, m2)
    where
        context2 = addToCtx context var (fromJust (lookupCtx c expr))
        (res, retCtx, retMem) = eval funExpr context2 m
        (i2, m2) = addToMem m (fromJust (lookupMem retMem (fromJust (lookupCtx retCtx var))))
        c2 = addToCtx c expr i2

eval (App (Function var funExpr context) expr) c m = (res, c2, m4)
    where
        (val, c2, m2) = eval (head expr) c m
        (i, m3) = addToMem m2 val
        context2 = addToCtx context var i
        (res, _, m4) = eval funExpr context2 m3

eval (Default) c m = (Number 1, c, m)

eval (Bool (Name name) e1 e2) c m = (calc name expr1 expr2, c3, m3)
    where
        (expr1, c2, m2) = eval e1 c m
        (expr2, c3, m3) = eval e2 c2 m2

eval (Case bool exprs) c m =
    if expr1 == (Number 1)
        then (expr2, c3, m3)
        else (expr3, c4, m4)
    where
        (expr1, c2, m2) = eval bool c m
        (expr2, c3, m3) = eval (head exprs) c2 m2
        (expr3, c4, m4) = eval (last exprs) c2 m2

eval (Set name expr) c m =
    if lookupCtx c name == Nothing
        then (val, addToCtx c2 name i, m3)
        else let (_, m4) = updateMem m2 (fromJust (lookupCtx c name)) val in (val, c, m4)
    where
        (val, c2, m2) = eval expr c m
        (i, m3) = addToMem m2 val

eval (Name name) c m =
    if index == Nothing
        then error ("Variable \"" ++ name ++ "\" not set")
        else (fromJust (lookupMem m (fromJust index)), c, m)
    where
        index = lookupCtx c name

eval (Lambda var expr) c m = ((Function var expr c), c, m)

eval (Block [x]) c m = eval x c m
eval (Block (x:xs)) c m = (eval (Block xs) c2 m2)
    where
        (_, c2, m2) = eval x c m

--- RUN

run :: String -> Ast
run str = expr where (expr, _, _) = eval (parse str) emptyCtx emptyMem