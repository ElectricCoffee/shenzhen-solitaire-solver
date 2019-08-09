-module(main).
-export([start/0, test/0]).
% re-defining the record for testing purposes. 
% It's a tuple internally anyway, so it shouldn't matter where it's defined
-record(card, {color, value}).

start() ->
    Deck = cards:shuffle(cards:make_deck()),
    io:format("~w~n", [Deck]).

%% @doc lazy unit tests via value assertions, replace with EUnit later.
test() ->
    Dragon = #card{color = green, value = dragon},
    Flower = #card{color = flower, value = flower},
    G2 = #card{color = green, value = 2},
    B2 = #card{color = black, value = 2},
    G3 = #card{color = green, value = 3},
    R4 = #card{color = red, value = 4},

    Unstackables = [cards:can_stack(A, B) || A <- [Dragon, Flower], B <- [Dragon, Flower, G2, B2, G3, R4]],
    false = lists:any(fun(A) -> A end, Unstackables),

    true = cards:can_stack(R4, G3),
    true = cards:can_stack(G3, B2),
    false = cards:can_stack(G3, G2),
    false = cards:can_stack(G2, B2),
    false = cards:can_stack(G3, R4),

    Deck = cards:make_deck(),
    Piles = cards:split_deck(Deck),
    true = lists:all(fun(X) -> X == 5 end, lists:map(fun length/1, Piles)),

    io:format("Tests passed!").