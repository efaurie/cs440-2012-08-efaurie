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

wrap2 f (x:y:xs) = (f x y):xs
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

-- Initial Dictionary

dictionary1 = dinsert "+" (Prim $ wrap2 (+)) H.empty
dictionary2 = dinsert "-" (Prim $ wrap2 (-)) dictionary1
dictionary3 = dinsert "*" (Prim $ wrap2 (*)) dictionary2
dictionary4 = dinsert "dup" (Prim dup) dictionary3
dictionary5 = dinsert "swap" (Prim swap) dictionary4
dictionary6 = dinsert "drop" (Prim Main.drop) dictionary5
dictionary7 = dinsert "rot" (Prim rot) dictionary6
dictionary = dinsert "/" (Prim $ wrap2 (div)) dictionary7
--want to get
--dinsert "/" (Prim $ wrap2 (div)) (dinsert ("*" (Prim $ wrap2 (*)) (
--dinsert "-" (Prim $ wrap2 (-)) (dinsert "+" (Prim $ wrap2 (+)) H.empty))))

-- The Evaluator

eval :: [String] -> ForthState -> IO ForthState
eval []    (istack, [],     dict) = return (istack, [], dict)
eval words (istack, cstack, dict) =
  case dlookup (head words) dict of
    Num i        -> eval xs (i:istack, cstack, dict)
    Prim f       -> eval xs (f istack, cstack, dict)
    Unknown "."  -> do { putStrLn $ show (head istack);
                             eval xs (tail istack, cstack, dict) }
    Unknown ".S"  -> do { printStack istack;
                         eval xs (istack, cstack, dict) }
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
