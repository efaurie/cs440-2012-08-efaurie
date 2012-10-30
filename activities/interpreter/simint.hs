{-# LANGUAGE NoMonomorphismRestriction #-}

import qualified Data.HashMap.Strict as H
import Text.ParserCombinators.Parsec
import System.Exit

--
-- The Data Types
--

data Exp = VarExp String
         | IntExp Int
         | OpExp String Exp Exp
         | DefExp String Exp
         | RelExp String Exp Exp
         | IfExp Exp Exp Exp
         | QuitExp
  deriving Show

data Val = IntVal Int
         | BoolVal Bool
  deriving (Show,Eq)

type Env = H.HashMap String Val

--
-- The Parser
--

symbol s = do string s
              spaces
              return s

pInt = do i <- many1 (oneOf ['0'..'9'])
          spaces
          return (IntExp (read i)) <?> "integer"

pVar = do first <- oneOf ['a'..'z']
          rest <- many (oneOf (concat [['a'..'z'],['0'..'9'],"'"]))
          spaces
          return (first : rest) <?> "identifier"

pVarE = do v <- pVar
           return (VarExp v) <?> "identifier"

mulOp = do     do { symbol "*"; return $ OpExp "*" }
           <|> do { symbol "/"; return $ OpExp "/" }

addOp = do     do { symbol "+"; return $ OpExp "+" }
           <|> do { symbol "-"; return $ OpExp "-" }

relOp = do     do { symbol "=="; return $ RelExp "==" }
           <|> do { symbol ">"; return $ RelExp ">" }
           <|> do { symbol "<"; return $ RelExp "<" }
           <|> do { symbol "<="; return $ RelExp "<=" }
           <|> do { symbol ">="; return $ RelExp ">=" }
           <|> do { symbol "/="; return $ RelExp "/=" }

pParens p = do symbol "("
               exp <- p
               symbol ")"
               return exp

pIf = do symbol "if"
         e1 <- pExp
         symbol "then"
         e2 <- pExp
         symbol "else"
         e3 <- pExp
         return $ IfExp e1 e2 e3

pDef = do symbol "def"
          spaces
          v <- pVar
          symbol "="
          e <- pExp
          return $ DefExp v e

pQuit = do symbol "quit"
           spaces
           return $ QuitExp

pExp = pTerm `chainl1` addOp
pTerm = pFact `chainl1` mulOp
pFact = pInt <|> pVarE <|> pParens pExp

pStmt = try pIf
        <|> try pDef
        <|> try pQuit
        <|> pExp

--
-- The Evaluator
--
relOpList = H.fromList [ ("==", (==))
                       , ("/=", (/=))
                       , (">", (>))
                       , (">=", (>=))
                       , ("<", (<))
                       , ("<=", (<=))
                       ]

opList = H.fromList [ ("+", (+))
                    , ("-", (-))
                    , ("*", (*))
                    , ("/", div)
                    ]

doRelOp op (IntVal v1) (IntVal v2) =
  let Just f = H.lookup op relOpList
   in BoolVal $ if (f v1 v2) then True 
                             else False

doOp op (IntVal i1) (IntVal i2) = 
  let Just f = H.lookup op opList
   in IntVal $ f i1 i2

doIf (BoolVal b1) (IntVal v1) (IntVal v2) =
  if b1 == True
    then IntVal v1
    else IntVal v2

eval :: Exp -> Env -> Val
eval (IntExp i) env = IntVal i
eval (VarExp v) env = 
   case H.lookup v env of
     Just i -> i
     Nothing -> IntVal 0
eval (OpExp op e1 e2) env =
   let v1 = eval e1 env
       v2 = eval e2 env
    in doOp op v1 v2
eval (RelExp op e1 e2) env =
   let v1 = eval e1 env
       v2 = eval e2 env
    in doRelOp op v1 v2
eval (IfExp e1 e2 e3) env =
   let v1 = eval e1 env
       v2 = eval e2 env
       v3 = eval e3 env
    in doIf v1 v2 v3

repl env = do
   putStr "> "
   line <- getLine
   let eitherExp = runParser pStmt "interpreter" "stdin" line
   case eitherExp of
     Right (DefExp v e) -> repl (H.insert v (eval e env) env)
     Right (QuitExp) -> do putStrLn "Goodbye!"
                           exitSuccess
     Right e -> do putStrLn $ show (eval e env)
                   repl env
     Left _ -> do putStrLn $ "You Idiot!"
                  repl env





















