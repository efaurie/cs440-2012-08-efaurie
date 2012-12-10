import Text.ParserCombinators.Parsec

data Exp = IntExp Integer
         | SymExp String
         | SExp [Exp]
         deriving (Show)

data Val = IntVal Integer
         | SymVal String
         | PrimVal ([Val] -> Val)

run x = parseTest x

runtime = [("+", liftIntOp (+) 0),
           ("-", liftIntOp (-) 0),
           ("*", liftIntOp (*) 1),
           ("/", liftIntOp div 1)]


strip (IntVal v)  = v
strip (SymVal v) = 5

liftIntOp f b = PrimVal $ IntVal $ myFold f b

myFold f b = foldr f b $ map strip 

-- Lexical Definitions

adigit = oneOf ['0'..'9']
asymstart = oneOf ("-*+/:'?><=" ++ concat [['a'..'z'],['A'..'Z']])
asymrest = many (asymstart <|> adigit)

-- Lexicals

digits = do num <- many1 adigit
            spaces
            return num

vars = do first <- asymstart
          rest <- asymrest
          spaces
          return ([first] ++ rest)

form = do char '('
          exps <- many anExp
          char ')'
          return exps

-- Grammaticals

anInt = do d <- digits
           return $ IntExp (read d)

aSym = do s <- vars
          return $ SymExp s

aForm = do e <- form
           return $ SExp e

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
eval (SExp e) env = do case (eval (head e) env) of
                         PrimVal v -> do let values = [(eval x env) | x <- (tail e)]
                                         v values
                         otherwise -> error "Something bad happened"
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
