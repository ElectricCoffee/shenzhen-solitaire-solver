-module(cards).
-export([make_deck/0, shuffle/1, can_stack/2]).

-record(card, {color, value}).

%% @doc creates a Shenzhen Solitaire deck of cards
make_deck() ->
    % The three available colours
    Colors = [red, green, black],
    % Each colour/suit has the cards 1-9 + 3 dragons
    Values = lists:seq(1, 9) ++ [dragon, dragon, dragon],
    % There's also a special flower card which isn't a dragon and isn't red, green, or black
    Flower = #card{color = flower, value = flower},
    Deck = [#card{color = Color, value = Value} || Color <- Colors, Value <- Values],
    [Flower | Deck].

%% @doc shuffles a list
shuffle(List) when is_list(List) -> 
    % Generates a list of tuples {Rand, Item} where Rand is a random value
    Prefixed = [{rand:uniform(), Item} || Item <- List],
    % It then sorts the list of tuples, and discards the random value for a shuffled list
    [Out || {_, Out} <- lists:sort(Prefixed)].

%% @doc checks if two cards can be stacked, where Card2 is the card that will be placed on top of Card1
can_stack(Card1, Card2) ->
    #card{color = Color1, value = Value1} = Card1,
    #card{color = Color2, value = Value2} = Card2,
    if 
        % dragons can never stack
        (Value1 == dragon) or (Value2 == dragon) -> false;
        % flowers can't either
        (Value1 == flower) or (Value2 == flower) -> false;
        % same colours also can't stack
        Color1 == Color2 -> false;
        % value 2 must be one less than value 1
        Value1 /= Value2 - 1 -> false;
        % otherwise, card 2 can be stacked on top of card 1
        true -> true
    end.