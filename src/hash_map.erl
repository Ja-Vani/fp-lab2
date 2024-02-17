-module(hash_map).

-include("hash_map.hrl").

-export([new/0, add_element/3, delete_element/2, filter/2, map/2, fold/3, merge/2,
         is_equal/2, get_value/2, is_map/1, get_size/1, from_list/2]).

new() ->
    #map{storage = array:new(?INIT_CAPACITY, {default, dict:new()})}.

calc_hash(Key, Array) ->
    erlang:phash2(Key, array:size(Array)).

add_element(Key, Value, #map{storage = Array}) ->
    Hash = calc_hash(Key, ?INIT_CAPACITY),
    Branch = array:get(Hash, Array),
    New_branch = dict:append(Key, Value, Branch),
    #map{storage = array:set(Hash, New_branch, Array)}.

delete_element(Key, #map{storage = Array}) ->
    Hash = calc_hash(Key, ?INIT_CAPACITY),
    Branch = array:get(Hash, Array),
    New_branch = dict:erase(Key, Branch),
    #map{storage = array:set(Hash, New_branch, Array)}.

filter(Pred, Map) ->
    #map{storage = Array} = Map,
    #map{storage = array:sparse_map(fun(_, Elem) -> dict:filter(Pred, Elem) end, Array)}.

map(Func, Map) ->
    #map{storage = Array} = Map,
    #map{storage = array:sparse_map(fun(_, Elem) -> dict:map(Func, Elem) end, Array)}.

fold(Func, InitAcc, Map) ->
    #map{storage = Array} = Map,
    array:sparse_foldl(fun(_, Elem, Acc) -> dict:fold(Func, Acc, Elem) end, InitAcc, Array).

merge(Map1, Map2) ->
    #map{storage = Array2} = Map2,
    array:sparse_foldl(fun(_, {Key2, Value2}, Acc) ->
                          add_element(Key2, Value2, delete_element(Key2, Acc))
                       end,
                       Map1,
                       Array2).

get_value(Key, Map) ->
    #map{storage = Array} = Map,
    Hash = calc_hash(Key, ?INIT_CAPACITY),
    Branch = array:get(Hash, Array),
    dict:get(Key, Branch).

get_size(Map) ->
    fold(fun(_, _, Acc) -> Acc + 1 end, 0, Map).

is_equal(Map1, Map2) ->
    #map{storage = Array1} = Map1,
    Size1 = get_size(Map1),
    Size2 = get_size(Map2),
    case Size1 == Size2 of
        true ->
            array:sparse_foldl(fun(_, {Key, Value}, Acc) ->
                                  case get_value(Key, Map2) of
                                      Value ->
                                          Acc;
                                      _ ->
                                          false
                                  end
                               end,
                               true,
                               Array1);
        fasle ->
            false
    end;
is_equal(_, _) ->
    false.

is_map(#map{storage = Array}) ->
    array:is_array(Array);
is_map(_) ->
    false.

from_list(Map, [{Key, Value} | Tail]) ->
    from_list(add_element(Key, Value, Map), Tail);
from_list(Map, []) ->
    Map.