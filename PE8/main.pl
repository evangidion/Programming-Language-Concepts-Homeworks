:- module(main, [is_movie_directed_by/2, total_awards_nominated/2, all_movies_directed_by/2, total_movies_released_in/3, all_movies_released_between/4]).
:- [kb].

% DO NOT CHANGE THE UPPER CONTENT, WRITE YOUR CODE AFTER THIS LINE

is_movie_directed_by(T, D) :- movie(T, D, _, _, _, _).

total_awards_nominated(T, N) :- movie(T, _, _, N1, N2, N3), N is N1 + N2 + N3.

all_movies_directed_by(D, Movies) :- findall(T, movie(T, D, _, _, _, _), Movies).

total_movies_released_in(M, Y, C) :- total_movies_helper(M, Y, 0, C).

total_movies_helper([], _, Acc, Acc).
total_movies_helper([A|B], Y, Acc, C) :- 
    movie(A, _, Y, _, _, _),
    Z is Acc + 1,
    total_movies_helper(B, Y, Z, C).
total_movies_helper([A|B], Y, Acc, C) :- 
    movie(A, _, Y1, _, _, _),
    Y =\= Y1,
    total_movies_helper(B, Y, Acc, C).

all_movies_released_between([], _, _, []).
all_movies_released_between([A|B], MY, MAY, MGY) :- 
    movie(A, _, Y, _, _, _),
    MY =< Y, 
    Y =< MAY,
    append([A], RMGY, MGY),
    all_movies_released_between(B, MY, MAY, RMGY).
all_movies_released_between([A|B], MY, MAY, MGY) :-
    movie(A, _, Y, _, _, _),
    Y < MY,
    all_movies_released_between(B, MY, MAY, MGY).
all_movies_released_between([A|B], MY, MAY, MGY) :-
    movie(A, _, Y, _, _, _),
    Y > MAY,
    all_movies_released_between(B, MY, MAY, MGY).
    