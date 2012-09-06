--Add, which takes a value x and adds it to the list xx
add x xx = x:xx

--Union, takes the union of xx and yy
union xx yy = xx ++ [y | y <- yy, y `notElem` xx]

--Intersect, takes the intersection of xx and yy
intersect xx yy = [x | x <- xx, x `elem` yy]

--powerset, returns the powerset of xx
powerset [] = [[]]
powerset (x:xs) = concat [ [s, x:s] | s <- powerset xs ]

--fibonacci direct style
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x-1) + fibonacci (x-2)

--fibonacci tail style
fib n = tailFib n (0,1)
tailFib n (a, b) = (if n == 0 then a else tailFib (n-1) (b, a+b))
