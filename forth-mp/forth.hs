import Data.HashMap.Strict as H

-- Initial types

type ForthState = (IStack, CStack, Dictionary)

type IStack = [Integer]
initialIStack = []

type CStack = [[String]]
initialCStack = []

-- Type for the symbol dictionary

type Dictionary = H.HashMap String [Entry]

data Entry =
     Prim ([Integer] -> [Integer])
   | Def [String]
   | Num Integer
   | Unknown String

instance Show Entry where
  show (Prim f)    = "Prim"
  show (Def s)     = show s
  show (Num i)     = show i
  show (Unknown s) = "Unknown: " ++ s

-- Dictionary helpers
--this causes problems with the writeup as to order of eval
-- original: wrap2 f (x:y:xs) = (f x y):xs
wrap2 f (x:y:xs) = (f y x):xs
wrap2 f _ = error "Value stack underflow!"

dlookup :: String -> Dictionary -> Entry
dlookup word dict =
  case H.lookup word dict of
    Nothing -> case reads word of
                 [(i,"")] -> Num i
                 _        -> Unknown word
    Just x  -> head x

dinsert :: String -> Entry -> Dictionary -> Dictionary
dinsert key val dict =
   case H.lookup key dict of
      Nothing -> H.insert key [val] dict
      Just x  -> H.insert key (val:x) dict

dinsert' (key, val, dict) = 
   case H.lookup key dict of
      Nothing -> H.insert key [val] dict
      Just x -> H.insert key (val:x) dict

--My Helper Functions
printStack [] = error "The stack is empty!"
printStack xs = print (myReverse xs)

myReverse list = myReverse' list []
   where
     myReverse' [] reversed = reversed
     myReverse' (x:xs) reversed = myReverse' xs (x:reversed)

dup [] = error "Stack is empty! No element to duplicate!"
dup [x] = [x] ++ [x]
dup (x:xs) = x:x:xs

swap [] = error "No elements in the stack! Can't swap!"
swap [x] = [x]
swap [x,y] = [y,x]
swap (x:y:xs) = y:x:xs

drop [] = error "No elements in the stack! Can't drop!"
drop [x] = []
drop (x:xs) = xs

rot [] = error "No elements in the list! Can't rotate!"
rot [x] = [x]
rot xs = (last xs):(init xs)

wrap2C f (x:y:xs) = if (f y x) == True
                      then [-1] ++ xs
                      else [0] ++ xs
wrap2C f _ = error "Value stack underflow!"

--takes a string and returns a string of all tokens before the ;
grabArgs [] = error "Didn't find a semicolon, ran out of arguments!"
grabArgs [x] = if x /= ";"
                then [x]
                else []
grabArgs (x:xs) = if x /= ";"
                    then [x] ++ grabArgs xs
                    else []

--remove block takes a list and returns the list after the semicolon
removeBlock [] = error "Didn't find a semicolon, ran out of arguments!"
removeBlock [x] = if x /= ";"
                    then error "Didn't find a semicolon, ran out of arguments!"
                    else []
removeBlock (x:xs) = if x /= ";"
                       then removeBlock xs
                       else (tail xs)


-- Initial Dictionary

dictionary1 = dinsert "+" (Prim $ wrap2 (+)) H.empty
dictionary2 = dinsert "-" (Prim $ wrap2 (-)) dictionary1
dictionary3 = dinsert "*" (Prim $ wrap2 (*)) dictionary2
dictionary4 = dinsert "dup" (Prim dup) dictionary3
dictionary5 = dinsert "swap" (Prim swap) dictionary4
dictionary6 = dinsert "drop" (Prim Main.drop) dictionary5
dictionary7 = dinsert "rot" (Prim rot) dictionary6
dictionary8 = dinsert "<" (Prim $ wrap2C (<)) dictionary7
dictionary9 = dinsert ">" (Prim $ wrap2C (>)) dictionary8
dictionary10 = dinsert "/=" (Prim $ wrap2C ((/=))) dictionary9
dictionary11 = dinsert "==" (Prim $ wrap2C ((==))) dictionary10
dictionary12 = dinsert "<=" (Prim $ wrap2C ((<=))) dictionary11
dictionary13 = dinsert ">=" (Prim $ wrap2C ((>=))) dictionary12
dictionary = dinsert "/" (Prim $ wrap2 (div)) dictionary13
--want to get
--dinsert "/" (Prim $ wrap2 (div)) (dinsert ("*" (Prim $ wrap2 (*)) (
--dinsert "-" (Prim $ wrap2 (-)) (dinsert "+" (Prim $ wrap2 (+)) H.empty))))
--Should be able to use some form of HOF or something

-- The Evaluator

eval :: [String] -> ForthState -> IO ForthState
eval []    (istack, [],     dict) = return (istack, [], dict)
eval words (istack, cstack, dict) =
  case dlookup (head words) dict of
    Num i        -> eval xs (i:istack, cstack, dict)
    Prim f       -> eval xs (f istack, cstack, dict)
    --if its a user defined word
    --leave the integer stack alone, add word to call stack
    Def str      -> eval (str ++ xs) (istack, cstack, dict)
    Unknown "."  -> do { putStrLn $ show (head istack);
                             eval xs (tail istack, cstack, dict) }
    Unknown ".S" -> do { printStack istack;
                         eval xs (istack, cstack, dict) }
    Unknown ":"  -> do { let name = head xs
                             args = grabArgs (tail xs)
                             rs = removeBlock (tail xs)
                         in eval rs (istack, cstack, (dinsert name (Def args) dict)) } 
  where xs = tail words

repl :: ForthState -> IO ForthState
repl state =
  do putStr "> " ;
     input <- getLine
     nustate <- eval (words input) state
     repl nustate

main = do
  putStrLn "Welcome to your forth interpreter!"
  repl (initialIStack, initialCStack, dictionary)
