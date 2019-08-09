-module(cards).
-export([make_deck/0, shuffle/1]).

-record(card, {color, value}).

make_deck() ->
    % The three available colours
    Colors = [red, green, black],
    % Each colour/suit has the cards 1-9 + 3 dragons
    Values = lists:seq(1, 9) ++ [dragon, dragon, dragon],
    % There's also a special flower card which isn't a dragon and isn't red, green, or black
    Flower = #card{color = flower, value = flower},
    Deck = [#card{color = Color, value = Value} || Color <- Colors, Value <- Values],
    [Flower | Deck].

shuffle(List) when is_list(List) -> 
    % Generates a list of tuples {Rand, Item} where Rand is a random value
    Prefixed = [{rand:uniform(), Item} || Item <- List],
    % It then sorts the list of tuples, and discards the random value for a shuffled list
    [Out || {_, Out} <- lists:sort(Prefixed)].