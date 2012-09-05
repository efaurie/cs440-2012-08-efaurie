-- Block 1
eval [] (x:_) = x

--Block 2

eval (0:(-1):cc) (a:b:yy) = eval cc (b : yy)
eval (0:1:cc) (a:b:yy) = eval cc (a + b : yy)
eval (0:2:cc) (a:b:yy) = eval cc (a - b : yy)
eval (0:3:cc) (a:b:yy) = eval cc (a * b : yy)

-- Block 3

eval (x:cc) yy = eval cc (x:yy)

-- Block 4

calc xx = eval xx []

