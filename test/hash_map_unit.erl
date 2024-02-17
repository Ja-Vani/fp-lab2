-module(hash_map_unit).

-include_lib("src/hash_map.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([add_thousand_elems/2]).

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

is_map_func_test() ->
    Array = array:from_list([{"a", 1}, {"b", 2}]),

    ?assert(hash_map:is_map(
                hash_map:new())),
    ?assert(not hash_map:is_map({Array, 2})),
    ?assert(not hash_map:is_map({Array, "2"})),
    ?assert(not hash_map:is_map({{"a", 1}, 1})),
    ?assert(not hash_map:is_map([Array, 2])).

add_element_test() ->
    Map = hash_map:new(),
    With1Elem = hash_map:add_element("a", 1, Map),
    With2Elems = hash_map:add_element(["b", "c"], atom, With1Elem),
    Size = hash_map:get_size(With2Elems),

    ?assertEqual(Size, 2).

grow_test() ->
    Map = add_thousand_elems(hash_map:new(), 0),

    ?assertEqual(hash_map:get_size(Map), 1000).

remove_elem_test() ->
    Map1 = add_thousand_elems(hash_map:new(), 0),
    %%  removed
    Map2 = hash_map:remove_elem(1, Map1),
    %%  removed
    Map3 = hash_map:remove_elem(404, Map2),
    %%  not remove (not exists)
    Map4 = hash_map:remove_elem(1000, Map3),

    ?assertEqual(hash_map:get_size(Map4), 998),
    ?assertEqual(error, hash_map:get_value(1, Map4)),
    ?assertEqual(error, hash_map:get_value(404, Map4)),
    ?assertEqual(error, hash_map:get_value(1000, Map4)).

filter_elems_test() ->
    Map1 = add_thousand_elems(hash_map:new(), 0),
    Map2 =
        hash_map:filter(fun({Key, Value}) -> (Key == 1) or (Value == 999) or (Value == 501) end,
                        Map1),
    Map3 = hash_map:filter(fun({Key, Value}) -> (Key == -1) or (Value == 1000) end, Map1),

    ?assertEqual(3, hash_map:get_size(Map2)),
    ?assertEqual(0, hash_map:get_size(Map3)),
    ?assertEqual(1, hash_map:get_value(1, Map2)),
    ?assertEqual(999, hash_map:get_value(999, Map2)),
    ?assertEqual(501, hash_map:get_value(501, Map2)),
    ?assertEqual(error, hash_map:get_value(502, Map2)),
    ?assertEqual(error, hash_map:get_value(1, Map3)).

map_elems_test() ->
    Map1 = add_thousand_elems(hash_map:new(), 0),
    Map2 = hash_map:map(fun({Key, Value}) -> Value * 2 + Key end, Map1),

    ?assertEqual(hash_map:get_size(Map2), 1000),
    ?assertEqual(3, hash_map:get_value(1, Map2)),
    ?assertEqual(1533, hash_map:get_value(511, Map2)),
    ?assertEqual(2997, hash_map:get_value(999, Map2)).

fold_elems_test() ->
    Map1 = add_thousand_elems(hash_map:new(), 0),
    Folded = hash_map:fold(fun({_, Value}, Acc) -> Acc + Value end, 0, Map1),
    ?assertEqual(499500, Folded).

merge_test() ->
    Map1 = add_thousand_elems(hash_map:new(), 0),
    Right = hash_map:map(fun({_, Value}) -> Value * 2 end, Map1),
    Left = hash_map:filter(fun({_, Value}) -> Value rem 2 == 0 end, Map1),
    Merged = hash_map:merge(Left, Right),

    ?assertEqual(1000, hash_map:get_size(Merged)),
    ?assertEqual(1002, hash_map:get_value(501, Right)),
    ?assertEqual(500, hash_map:get_value(500, Left)).

merge_associativity_test() ->
    Map1 = add_thousand_elems(hash_map:new(), 0),
    Left = hash_map:filter(fun({Key, _}) -> Key rem 2 == 0 end, Map1),
    Map2 = hash_map:map(fun({_, Value}) -> Value * 2 end, Map1),
    %%  With shared key 2
    Right = hash_map:filter(fun({Key, _}) -> (Key rem 2 == 1) or (Key == 2) end, Map2),
    Middle = hash_map:filter(fun({Key, _}) -> Key rem 3 == 2 end, Map2),
    MergedLeft2Right = hash_map:merge(Left, hash_map:merge(Middle, Right)),
    MergedRight2Left =
        hash_map:merge(
            hash_map:merge(Left, Middle), Right),

    ?assert(hash_map:is_equal(MergedLeft2Right, MergedRight2Left)).

merge_neutral_elem_test() ->
    Map1 = add_thousand_elems(hash_map:new(), 0),
    Map2 = hash_map:from_list([{"", 1}, {"", 2}]),
    EmptyMap = hash_map:new(),
    MergedLeft = hash_map:merge(Map1, EmptyMap),
    MergedRight = hash_map:merge(EmptyMap, Map1),

    ?assert(hash_map:is_equal(Map2, hash_map:merge(Map2, EmptyMap))),
    ?assert(hash_map:is_equal(Map2, hash_map:merge(EmptyMap, Map2))),
    ?assert(hash_map:is_equal(Map1, MergedLeft)),
    ?assert(hash_map:is_equal(MergedLeft, MergedRight)).

add_thousand_elems(Map, Count) when Count < 1000 ->
    add_thousand_elems(hash_map:add_element(Count, Count, Map), Count + 1);
add_thousand_elems(Map, _) ->
    Map.
