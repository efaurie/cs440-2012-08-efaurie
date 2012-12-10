import Text.ParserCombinators.Parsec

data Exp = IntExp Integer
         | SymExp String
         | OpExp ([Expr] -> Expr)
         | SExp [Exp]
         deriving (Show)

data Val = IntVal Integer
         | SymVal String
         | PrimVal ([Val] -> Val)


runtime = [("+", liftIntOp (+) 0),
           ("-", liftIntOp (-) 0),
           ("*", liftIntOp (*) 1),
           ("/", liftIntOp div 1)]

liftIntOp f b xx = foldr f b xx
 

-- Lexical Definitions

adigit = oneOf ['0'..'9']
asymstart = oneOf (":'?><=" ++ concat [['a'..'z'],['A'..'Z']])
asymrest = many (asymstart <|> adigit)
opslist = oneOf ("-*+/")

-- Lexicals

digits = do num <- many1 adigit
            spaces
            return num

vars = do first <- asymstart
          rest <- asymrest
          spaces
          return ([first] ++ rest)

ops = do op <- opslist
         spaces
         return op

form = do char '('
          exps <- many anExp
          char ')'
          spaces
          return exps

-- Grammaticals

anInt = do d <- digits
           return $ IntExp (read d)

aSym = do s <- vars
          return $ SymExp s

aForm = do e <- form
           return $ SExp e

anOp = do o <- ops
          case (lookup o runtime) of
               Just n -> return n
               Nothing -> error "Unknown Argument"

anAtom = try anInt
         <|> aSym

anExp = try aForm
        <|> anAtom

-- Evaluator

eval :: Exp -> [(String,Val)] -> Val
eval (IntExp i) env = IntVal i
eval (SymExp s) env = do case (lookup s env) of
                           Just n -> n
                           Nothing -> error ("Symbol " ++ s ++ " has no value.")
eval (OpExp o) env = do case (lookup o env) of
                           Just n -> n
                           Nothing -> error "Unknown Operator"
eval (SExp e) env = do case (eval (head e) env) of
                         PrimVal v -> do let values = [(eval x env) | x <- (tail e)]
                                         v values
                         otherwise -> error "Something bad happened"

eval (SExp e) env = apply (eval (head e) env) (map eval (tail e))
                    where apply :: Expr -> [Expr] -> Expr
                          apply (OpExp f) args = f args
-- Printer

instance Show Val where
  show (IntVal i) = show i
  show (SymVal s) = s

repl defs =
  do putStr "> "
     l <- getLine
     case parse anExp "Expression" l of
       Right exp -> putStr (show (eval exp defs))
       Left pe   -> putStr (show pe)
     putStrLn ""
     repl defs
