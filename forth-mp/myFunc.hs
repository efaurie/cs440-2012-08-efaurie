printStack [] = error "The stack is empty!"
printStack xs = print (myReverse xs)

myReverse list = myReverse' list []
  where
    myReverse' [] reversed = reversed
    myReverse' (x:xs) reversed = myReverse' xs (x:reversed)

dup (x:xs) = x:x:xs

swap (x:y:xs) = y:x:xs

myDrop (x:xs) = xs

rot xs = (last xs):(init xs)  
