-module(hash_map_property).

-include_lib("src/hash_map.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PROPERTY_TESTS_AMOUNT, 100).

-export([get_property_test_result/1]).

get_property_test_result(Property) ->
    proper:quickcheck(Property, [{numtests, ?PROPERTY_TESTS_AMOUNT}, {to_file, user}]).

%%--------------------------------------------------------------------
%% PROPERTIES
%%--------------------------------------------------------------------

prop_insert_invariant() ->
    ?FORALL({L, {K, V}},
            {list({any(), any()}), {any(), any()}},
            ?IMPLIES(hash_map:get_value(K,
                                        hash_map:from_list(
                                            hash_map:new(), L))
                     =:= error,
                     begin
                         Map = hash_map:from_list(
                                   hash_map:new(), L),
                         New = hash_map:add_element(K, V, Map),
                         (hash_map:get_size(Map) + 1 == hash_map:get_size(New))
                         and (hash_map:get_value(K, New) == V)
                     end)).

prop_remove_invariant() ->
    ?FORALL({L, K},
            {list({string(), any()}), string()},
            ?IMPLIES(hash_map:get_value(K,
                                        hash_map:from_list(
                                            hash_map:new(), L))
                     =/= error,
                     begin
                         Map = hash_map:from_list(
                                   hash_map:new(), L),
                         New = hash_map:delete_element(K, Map),
                         OccupiedCondition =
                             (hash_map:get_size(Map) == 0)
                             or (hash_map:get_size(Map) - 1 == hash_map:get_size(New))
                                and (hash_map:get_size(Map) > 0),
                         OccupiedCondition and (hash_map:get_value(K, New) =:= error)
                     end)).

prop_merge_associativity_invariant() ->
    ?FORALL({LeftList, MiddleList, RightList},
            {list({any(), any()}), list({any(), any()}), list({any(), any()})},
            begin
                Left =
                    hash_map:from_list(
                        hash_map:new(), LeftList),
                Middle =
                    hash_map:from_list(
                        hash_map:new(), MiddleList),
                Right =
                    hash_map:from_list(
                        hash_map:new(), RightList),
                MergedLeft2Right =
                    hash_map:merge(
                        hash_map:merge(Left, Middle), Right),
                MergedRight2Left = hash_map:merge(Left, hash_map:merge(Middle, Right)),
                hash_map:is_equal(MergedLeft2Right, MergedRight2Left)
            end).

prop_merge_neutral_elem_invariant() ->
    ?FORALL(L,
            list({any(), any()}),
            begin
                EmptyMap = hash_map:new(),
                Map = hash_map:from_list(
                          hash_map:new(), L),
                MergedLeft = hash_map:merge(Map, EmptyMap),
                MergedRight = hash_map:merge(EmptyMap, Map),
                hash_map:is_equal(MergedLeft, Map) and hash_map:is_equal(MergedRight, Map)
            end).

%%--------------------------------------------------------------------
%% TESTS
%%--------------------------------------------------------------------

properties_test() ->
    ?assert(get_property_test_result(prop_insert_invariant())),
    ?assert(get_property_test_result(prop_remove_invariant())),
    ?assert(get_property_test_result(prop_merge_associativity_invariant())),
    ?assert(get_property_test_result(prop_merge_neutral_elem_invariant())).
