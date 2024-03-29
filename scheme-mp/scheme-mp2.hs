import Text.ParserCombinators.Parsec

data Exp = IntExp Integer
         | SymExp String
         deriving (Show)

data Val = IntVal Integer
         | SymVal String

run x = parseTest x

-- Lexicals

adigit = oneOf ['0'..'9']
digits = many1 adigit

avarstart = "-*+/:'?><=" ++ concat [['a'..'z'], ['A'..'Z']]

avar = do first <- oneOf avarstart
          rest <- many (oneOf (avarstart ++ ['0'..'9']))
          return ([first] ++ rest)

-- Grammaticals

anInt = do d <- digits
           return $ IntExp (read d)

aSym = do s <- avar
          return $ SymExp s

anAtom = try anInt
         <|> aSym

anExp = anAtom 

-- Evaluator

eval :: Exp -> [(String,Val)] -> Val
eval (IntExp i) env = IntVal i
eval (SymExp s) env = do case (lookup s env) of
                           Just n -> n
                           Nothing -> error ("Symbol " ++ s ++ " has no value.")

-- Printer

instance Show Val where
  show (IntVal i) = show i
  --show the loopup for s
  show (SymVal s) = s

repl defs =
  do putStr "> "
     l <- getLine
     case parse anExp "Expression" l of
       Right exp -> putStr (show (eval exp defs))
       Left pe   -> putStr (show pe)
     putStrLn ""
     repl defs
