-module(time).

-export([zero/0, inc/1, merge/2, clock/1, update/3, safe/2]).

zero() ->
    0.

inc(T) ->
    T + 1.

merge(Ti, Tj) -> 
    case leq(Ti, Tj) of
        true ->
            Tj;
        false ->
            Ti
    end.

leq(Ti, Tj) ->
    if
        Ti =< Tj ->
            true;
        Ti > Tj ->
            false
    end.

clock(Nodes) ->
    lists:map(fun(X) -> {X, 0} end, Nodes).

update(Node, Time, Clock) ->
    {_, Current} = lists:keyfind(Node, 1, Clock),
    Final = merge(Time, Current),
    lists:map(fun
        (Worker = {X, _}) -> 
            case X of
                Node ->
                    {Node, Final};
                _ ->
                    Worker
            end 
    end, Clock).


safe(Time, Clock) ->
    lists:foldl(fun({_, X}, Acc) -> 
            case Acc of
                false -> false;
                true -> 
                    if 
                        Time > X ->
                            false;
                        Time =< X ->
                            true
                    end
            end
        end, true, Clock).