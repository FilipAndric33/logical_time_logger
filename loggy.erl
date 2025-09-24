-module(loggy).
-export([start/1, stop/1]).

start(Nodes) ->
    spawn_link(fun() -> init(Nodes) end).

stop(Logger) -> 
    Logger ! stop.

init(Nodes) -> 
    loop(time:clock(Nodes), []).

loop(Clock, Queue) ->
    receive 
        {log, From, Time, Msg} ->
            NewC = time:update(From, Time, Clock),
            NewQ = case time:safe(Time, NewC) of
                true ->
                    {Safe, Rest} = lists:partition(fun({_, B, _}) ->
                            B =< Time
                        end, Queue ++ [{From, Time, Msg}]),
                    lists:foreach(fun({A, B, C}) -> 
                        log(A, B, C)
                    end, Safe),
                    Rest;
                false ->
                    Queue ++ [{From, Time, Msg}]
            end,
            loop(NewC, NewQ);
        stop ->
            ok
    end.

log(From, Time, Msg) ->
    io:format("log: ~w ~w ~p ~n", [From, Time, Msg]).