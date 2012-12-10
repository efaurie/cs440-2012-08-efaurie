{-# LANGUAGE NoMonomorphismRestriction #-}

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Error
import System.IO
import Data.IORef

--
-- Data Types
--
data Exp = IntExp Integer
         | SymExp String
	 | SExp [Exp]
	 | BoolExp String
         deriving (Show)

data Val = Atom String
	 | IntVal Integer
         | SymVal String
	 | PrimVal String
	 | List [Val]
	 | DottedList [Val] Val
	 | Bool Bool
	 | PrimitiveFunc ([Val} -> ThrowsError Val)
	 | Func {params :: [PrimVal], vararg :: (Maybe PrimVal),
		   body :: [Val], closure :: Env}
	 | IOFunc ([Val] -> IOThrowsError Val)
	 | Port Handle

data SError = NumArgs Integer [Val]
	    | TypeMismatch PrimVal Val
	    | Parser ParseError
	    | BadSpecialForm PrimVal Val
	    | NotFunction PrimVal PrimVal
	    | UnboundVar PrimVal PrimVal
	    | Default PrimVal

data Unpacker = forall a. Eq a => AnyUnpacker (Val -> ThrowsError a)

--
-- Environment
--

type Env = IOReg [(String, IORef LispVal)]

ioPrimitives :: [(PrimVal, [Val] -> IOThrowsError Val)]
ioPrimitives  = [("apply", applyProc
	        ,("open-input-file", makePort ReadMode)
	        ,("open-output-file", makePort WriteMode)
	        ,("close-input-port", closePort)
	        ,("close-output-port", closePort)
	        ,("read", readProc)
	        ,("write", writeProc)
	        ,("read-contents", readContents)
	        ,("read-all", readAll)
	        ]

primitives :: [(PrimVal, [Val] -> ThrowsError Val
primitives = [("+", numericBinop (+))
	     ,("-", numericBinop (-))
	     ,("*", numericBinop (*))
	     ,("/", numericBinop div)
	     ,("mod", numericBinop mod)
	     ,("quotient", numericBinop quot)
	     ,("remainder", numericBinop rem)
	     ,("=", numBoolBinop (==))
	     ,("<", numBoolBinop (<))
	     ,(">", numBoolBinop (>))
	     ,("/=", numBoolBinop (/=))
	     ,(">=", numBoolBinop (>=))
	     ,("<=", numBoolBinop (<=))
	     ,("&&", boolBoolBinop (&&))
	     ,("||", boolBoolBinop (||))
	     ,("string=?", strBoolBinop (==))
	     ,("string<?", strBoolBinop (<))
	     ,("string>?", strBoolBinop (>))
	     ,("string<=?", strBoolBinop (<=))
	     ,("string>=?", strBoolBinop (>=))
	     ,("car", car)
	     ,("cdr", cdr)
	     ,("cons", cons)
	     ,("eq?", eqv)
	     ,("eqv?", eqv)
	     ,("equal?", equal)]

numericBinop :: (IntVal -> IntVal -> IntVal) -> [Val] -> ThrowsError Val
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (Val -> ThrowsError a) -> (a -> a -> Bool) -> [Val] -> ThrowsError Val
boolBinop unpacker op args = if lenght args /= 2
			     then throwError $ NumArgs 2 args
			     else do left <- unpacker $ args !! 0
			     	     right <- unpacker $ args !! 1
				     return $ Bool $ left `op` right

unpackNum :: Val -> ThrowsError IntVal
unpackNum (IntVal n) = return n
unpackNum (PrimVal n) = let parsed = reads n in
			  if null parsed
			    then throwError $ TypeMismatch "number" $ PrimVal n
			    else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = throwError $ TypeMismatch "number" notNum

unpackStr :: Val -> ThrowsError PrimVal
unpackStr (PrimVal s) = return s
unpackStr (IntVal s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: Val -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

unpackEquals :: Val -> Val -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
	     do unpacked1 <- unpacker arg1
	        unpacked2 <- unpacker arg2
		return $ unpacked1 == unpacked2
	`catchError` (const $ return False)




--opList = [ ("+", liftIntOp (+))
--	 , ("-", liftIntOp (-))
--	 , ("*", liftIntOp (*))
--	 , ("/", liftIntOp div)
--	 ]

--runtime = [ ("+", PrimVal "+")
--	  , ("-", PrimVal "-")
--	  , ("*", PrimVal "*")
--	  , ("/", PrimVal "/")
--	  ]

--
-- List Funcitons
--

-- head
car :: [Val] -> ThrowsError Val
car [List (x:xs)] return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

--tail
cdr :: [Val] -> ThrowsError Val
cdr [List (x:xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [Val] -> ThrowsError Val
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [Val] -> ThrowsError Val
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(IntVal arg1), (IntVal arg2)] = return $ Bool $ arg1 == arg2
eqv [(PrimVal arg1), (PrimVal arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y) = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
   where eqvPair (x1,x2) = case eqv [x1,x2] of
   			      Left err -> False
			      Right (Bool val) -> val
eqv [_,_] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList



--
-- Parsers
--
run x = parseTest x

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

nullEnv :: IO Env
nullEnv = newIORef []

parseString :: Parser Val
parseString = do char '"'
	         x <- many (noneOf "\"")
		 char '"'
		 return $ String x

parseAtom :: Parser Val
parseAtom = do first <- letter <|> symbol
	       rest <- many (letter <|> digit <|> symbol)
	       let atom = first:rest
	       return $ case atom of
	       		  "#t" -> Bool True
			  "#f" -> Bool False
			  _    -> Atom atom

parseNumber :: Parser Val
parseNumber = liftM (Number . read) $ many1 digit

parseExpr :: Parser Val
parseExpr = parseAtom
	<|> parseString
	<|> parseNumber
	<|> parseQuoted
	<|> do char '('
	       x <- try parseList <|> parseDottedList
	       char ')'
	       return x

parseList :: Parser Val
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser Val
parseDottedList = do
   head <- endBy parseExpr spaces
   tail <- char '.' >> spaces >> parseExpr
   return $ DottedList head tail

parseQuoted :: Parser Val
parseQuoted = do
   char '\''
   x <- parseExpr
   return $ List [Atom "quote", x]



-- Random Necessities
adigit = oneOf ['0'..'9']
digits = many1 adigit
ws = many (oneOf " \t\n")

-- First Symbol Characters
pSym = do first <- oneOf $ "-*+/:?><=" ++ ['a'..'z'] ++ ['A'..'Z']
          rest  <- many (oneOf (['a'..'z'] ++ ['0'..'9'] ++ ['A'..'Z']))
	  ws
	  return (first : rest) <?> "Symbol y has no value."

liftIntOp f xx = IntVal $ foldr f 0 xx


anInt = do d <- digits
           return $ IntExp (read d)

--anSym = do 

anAtom = anInt

anExp = anAtom

aForm = do string "("
	   ws
	   e <- many anExp
	   ws
	   string ")"
	   return $ SExp e


-- Value Constructor




--
-- Evaluator
--

eval :: Env -> Val -> IOThrowsError Val
eval env val@(PrimVal _) = return val
eval env val@(IntVal _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List (Atom func : args)) = mapM eval args >>= apply func
eval env (List [Atom "if", pred, conseq, alt]) =
   do result <- eval env pred
      case result of
         Bool False -> eval env alt
	 otherwise -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) =
   eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = 
   eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
   makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
   makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
   makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
   makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
   makeVarargs varargs env [] body
eval env (List (function : args)) = do 
   func <- eval env function
   argVals <- mapM (eval env) args
   apply func argVals
eval env (List (Atom func : args)) = mapM (eval env) args >>= liftThrows . apply func
eval env badForm = throwsError $ BadSpecialForm "Unrecognized special form" badForm
eval env (List [Atom "load", PrimVal filename]) =
   load filename >>= liftM last . mapM (eval env)


--eval :: Exp -> [(String,Val)] -> Val
--eval (IntExp i) env = IntVal i
--eval (SymExp s) env = case lookup s env
--		      of   Just a  -> a
--		           Nothing -> error ("Symbol " ++ s ++ " has no value.")
--eval (SExp ((SymExp s):xs)) env = case lookup s env
--				  of   Just (PrimVal a) -> maybe (error "Error 1") (\b -> b xs) (lookup a opList)
--				       Nothing -> error ("Symbol " ++ s ++ " has no value.")


--
-- Printer
--

instance Show Val where show = showVal
instance Show SError where show = showError

instance Error SError where
   noMsg = Default "An error has occurred."
   strMsg = Default

type ThrowsError = Either SError
type IOThrowsError = ErrorT SError IO

showVal :: Val -> String
showVal (PrimVal contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (IntVal contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
   "(lambda (" ++ unwords (map show args) ++
      (case varargs of
         Nothing -> ""
	 Just args -> " . " ++ arg) ++ ") ...)"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"

showError :: SError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected
				  ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
				       ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

--
-- Repl
--

main :: IO ()
main = do args <- getArgs
	  if null args then runRepl else runOne $ args

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

runOne :: [PrimVal] -> IO ()
runOne args = do
   env <- primitiveBindings >>= flip bindVars [("args", List $ map PrimVal $ drop 1 args)]
   (runIOThrows $ liftM show $ eval env (List [Atom "load", PrimVal (args !! 0)]))
      >>= hPutStrLn stderr

runIOThrows :: IOThrowsError PrimVal -> IO PrimVal
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
					      ++ map (makeFunc PrimitiveFunc primitives)
   where makeFunc constructor (var, func) = (var, constructor func)

evalAndPrint :: Env -> PrimVal -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

evalString :: Env -> PrimVal -> IO PrimVal
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

load :: PrimVal -> IOThrowsError [Val]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readPrompt :: PrimVal -> IO PrimVal
readPrompt prompt = flushStr prompt >> getLine

readExpr :: readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)

readOrThrow :: Parser a -> PrimVal -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
   Left err -> throwError $ Parser err
   Right val -> return va

--repl defs =
--  do putStr "> "
--     l <- getLine
--     case parse anExp "Expression" l of
--       Right exp -> putStr (show (eval exp defs))
--       Left pe   -> putStr (show pe)
--     putStrLn ""
--     repl defs


--
-- Helper Functions
--

unwordsList :: [Val] -> PrimVal
unwordsList = unwords . map showVal

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

equal :: [Val] -> ThrowsError Val
equal [arg1, arg2] = do
   primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
   		     [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
   eqvEquals <- eqv [arg1, arg2]
   return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

flushStr :: PrimVal -> IO ()
flushStr str = putStr str >> hFlush stdout

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
   result <- prompt
   if pred result
      then return()
      else action result >> until_ pred prompt action

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

isBound :: Env -> PrimVal -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> PrimVal -> IOThrowsError Val
getVar envRef var = do env <- liftIO $ readIORef envRef
		       maybe (throwError $ UnboundVar "Getting an unbound variable" var)
		       	     (liftIO . readIORef)
			     (lookup var env)

setVar :: Env -> PrimVal -> Val -> IOThrowsError Val
setVar envRef var value = do env <- liftIO $ readIORef envRef
			     maybe (throwError $ UnboundVar "Setting an unbound variable" var)
			     	   (liftIO . (flip writeIORef value))
				   (lookup var env)
		 	     return value

defineVar :: Env -> PrimVal -> Val -> IOThrowsError Val
defineVar envRef var value = do
   alreadyDefined <- liftIO $ isBound envRef var
   if alreadyDefined
      then setVar envRef var value >> return value
      else liftIO $ do
         valueRef <- newIORef value
	 env <- readIORef envRef
	 writeIO envRef ((var, valueRef) : env)
	 return value

bindVars :: Env -> [(PrimVal, Val)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
   where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
         addBinding (var, value) = do ref <- newIORef value
	 			      return (var, ref)

apply :: Val -> [Val] -> IOThrowsError Val
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
   if num params /= num args && varargs == Nothing
      then throwError $ NumArgs (num params) args
      else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
   where remainingArgs = drop (length params) args
         num = toInteger . length
	 evalBody env = liftM last $ mapM (eval env) body
	 bindVarArgs arg env = case arg of
	    Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
	    Nothing -> return env
apply (IOFunc func) args = func args

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . showVal

applyProc :: [Val] -> IOThrowsError Val
applyProc [func, List args] = apply func args
applyProc (func:args) = apply func args

makePort :: IOMode -> [Val] -> IOThrowsError Val
makePort mode [PrimVal filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [Val] -> IOThrowsError Val
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool Flase

readProc :: [Val] -> IOThrowsError Val
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [Val] -> IOThrowsError Val
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [Val] -> IOThrowsError Val
readContents [PrimVal filename] = liftM PrimVal $ liftIO $ readFile filename

readAll :: [Val] -> IOThrowsError Val
readAll [PrimVal filename] = liftM List $ load filename
