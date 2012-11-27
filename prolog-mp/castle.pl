connected(alligatorpond,backdoor,2).
connected(backdoor,exit,1).
connected(bedroom,library,5).
connected(billiardroom,greenhouse,2).
connected(coatroom,servantroom,2).
connected(diningroom,alligatorpond,2).
connected(diningroom,kitchen,4).
connected(diningroom,library,7).
connected(entrance,mainhall,2).
connected(greenhouse,backdoor,5).
connected(kitchen,billiardroom,5).
connected(kitchen,greenhouse,1).
connected(library,billiardroom,3).
connected(mainhall,bedroom,4).
connected(mainhall,coatroom,3).
connected(mainhall,diningroom,3).
connected(mainhall,library,6).
connected(servantroom,alligatorpond,2).
connected(servantroom,diningroom,3).

append(Y, [], Y).
append(X, L, [X|L]).

pathfrom(X, Y, [X, Y]) :- connected(X, Y, _).
pathfrom(X, Y, L) :- connected(X, Z, _), pathfrom(Z, Y, L1), append(X, L1, L).

auxcost(X, Y, [X, Y], C) :- connected(X, Y, C).
auxcost(X, Y, L, C) :- connected(X, Z, C1), auxcost(Z, Y, L1, C2), append(X, L1, L), C is C1+C2.

costof(L, C) :- auxcost(X, Y, L, C).

costoflist([], []).
costoflist(P, C) :- costof(P, C).
costoflist([P|T], [C|CT]) :- costof(P, C), costoflist(T, CT).

leqlist(M, M, H) :- M =< H.
leqlist(H, M, H) :- M > H.

minlist(X, [X]).
minlist(M, [X|Y]) :- minlist(M1, Y), leqlist(M, M1, X).

mincost(P, C, [P|_], [C|CT]) :- minlist(C, [C|CT]).
mincost(P, C, [_|PT], [_|CT]) :- minlist(C, CT), mincost(P, C, PT, CT).

findpath(A, B, P, C) :- costof(P, C), findall(L, pathfrom(A, B, L), PL), costoflist(PL, CL), mincost(P, C, PL, CL).

member(M, [M|_]).
member(M, [_|R]) :- member(M,R).

goodpath(L) :- not(member(alligatorpond, L)), member(P, L), member(P, [servantroom, billiardroom]).

findgoodpath(A, B, P, C) :- costof(P, C), findall(X, (pathfrom(A, B, X), goodpath(X)), L), costoflist(L, R), mincost(P,C,L,R).
