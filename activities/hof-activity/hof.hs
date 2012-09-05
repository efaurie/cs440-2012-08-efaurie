-- mymap

mymap f xx = [f x | x <- xx]

--flatmap
flatmap f [x] = [f x]
flatmap f (x:xs) = [f x] ++ flatmap f xs


