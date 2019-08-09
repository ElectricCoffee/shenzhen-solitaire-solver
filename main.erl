-module(main).
-export([start/0]).

start() ->
    Deck = cards:shuffle(cards:make_deck()),
    io:format("~w~n", [Deck]).