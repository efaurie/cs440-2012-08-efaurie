--Activity 3 -- Higher Order Functions
--Eric Faurie

-- mymap
mymap f xx = [f x | x <- xx]

--flatmap
flatmap f [x] = [f x]
flatmap f (x:xs) = [f x] ++ flatmap f xs

--myfoldr
myfoldr f b [y] = f y b 
myfoldr f b (x:xs) = f x (myfoldr f b xs)

--myfoldl
myfoldl f b [y] = f b y
myfoldl f b (x:xs) = f (myfoldl f b xs) x

--myzipwith
myzipwith f [x] [y] = [f x y]
myzipwith f (x:xs) (y:ys) = [f x y] ++ (myzipwith f xs ys)

--exists
exists f xx = True `elem` mymap f xx

--forall
forall f xx = if False `elem` mymap f xx
                then False
                else True

--myfilter
myfilter f [x] = if f x
                   then [x]
                   else []
myfilter f (x:xs) = if f x
                      then [x] ++ myfilter f xs
                      else [] ++ myfilter f xs 

--inclist
inclist xx = mymap (+ 1) xx

--doublelist
doublelist xx = mymap (*2) xx

--sumlist
sumlist xx = myfoldr (+) 0 xx

--prodlist
prodlist xx = myfoldr (*) 1 xx

--sumpairs
sumpairs xx yy= myzipwith (+) xx yy

--intersect
intersect xx yy = [x | x <- xx, x `elem` yy]

--pow
prefix x xx = mymap (x:) xx
pow = foldr(\x ys -> ys ++ (prefix x ys)) [[]]

--nats
nats = [1,2..]

--fib
fib = 0 : 1 : zipWith (+) fib (tail fib)
