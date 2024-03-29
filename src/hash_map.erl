-module(hash_map).

-include("hash_map.hrl").

-export([new/0, add_element/3, delete_element/2, filter/2, map/2, fold/3, merge/2,
         is_equal/2, get_value/2, is_map/1, get_size/1, from_list/2]).

new() ->
    #map{storage = array:new(?INIT_CAPACITY, {default, []})}.

calc_hash(Key, Array) ->
    erlang:phash2(Key, array:size(Array)).

add_element(Key, Value, #map{storage = Array}) ->
    Hash = calc_hash(Key, Array),
    Branch = array:get(Hash, Array),
    Newbranch =
        case lists:keyfind(Key, 1, Branch) of
            false ->
                [{Key, Value} | Branch];
            _ ->
                lists:keyreplace(Key, 1, Branch, {Key, Value})
        end,
    #map{storage = array:set(Hash, Newbranch, Array)}.

delete_element(Key, #map{storage = Array}) ->
    Hash = calc_hash(Key, Array),
    Branch = array:get(Hash, Array),
    Newbranch = lists:keydelete(Key, 1, Branch),
    #map{storage = array:set(Hash, Newbranch, Array)}.

filter(Pred, Map) ->
    #map{storage = Array} = Map,
    #map{storage = array:sparse_map(fun(_, Elem) -> lists:filter(Pred, Elem) end, Array)}.

map(Func, Map) ->
    #map{storage = Array} = Map,
    #map{storage = array:sparse_map(fun(_, Elem) -> lists:map(Func, Elem) end, Array)}.

fold(Func, InitAcc, Map) ->
    #map{storage = Array} = Map,
    array:sparse_foldl(fun(_, Elem, Acc) -> lists:foldl(Func, Acc, Elem) end, InitAcc, Array).

merge(Map1, Map2) ->
    #map{storage = Array2} = Map2,
    array:sparse_foldl(fun(_, Dict, Acc) ->
                          lists:foldl(fun({Key2, Value2}, Acc1) ->
                                         add_element(Key2, Value2, delete_element(Key2, Acc1))
                                      end,
                                      Acc,
                                      Dict)
                       end,
                       Map1,
                       Array2).

get_value(Key, Map) ->
    #map{storage = Array} = Map,
    Hash = calc_hash(Key, Array),
    Branch = array:get(Hash, Array),
    Element = lists:keyfind(Key, 1, Branch),
    case Element of
        {_, Val} ->
            Val;
        _ ->
            error
    end.

get_size(Map) ->
    fold(fun(_, Acc) -> Acc + 1 end, 0, Map).

is_equal_branch(false, _, _) ->
    false;
is_equal_branch(Acc, Map2, Branch) ->
    lists:foldl(fun({Key, Value}, Accd) ->
                 case get_value(Key, Map2) of
                     Value ->
                         Accd;
                     _ ->
                         false
                 end
              end,
              Acc,
              Branch).

is_equal(Map1, Map2) ->
    #map{storage = Array1} = Map1,
    Size1 = get_size(Map1),
    Size2 = get_size(Map2),
    case Size1 == Size2 of
        true ->
            array:sparse_foldl(fun(_, Branch, Acc) -> is_equal_branch(Acc, Map2, Branch) end,
                               true,
                               Array1);
        _ ->
            false
    end.

is_map(#map{storage = Array}) ->
    array:is_array(Array);
is_map(_) ->
    false.

from_list(Map, []) ->
    Map;
from_list(Map, [{Key, Value} | Tail]) ->
    from_list(add_element(Key, Value, Map), Tail).
