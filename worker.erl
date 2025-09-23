-module(worker).

-export([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
    spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
    Worker ! stop.

init(Name, Logger, Seed, Sleep, Jitter) ->
    random:seed(Seed, Seed, Seed),
    receive
        {peers, Peers} ->
            Time = time:zero(),
            loop(Name, Logger, Peers, Time, Sleep, Jitter);
        stop ->
            ok
    end.

peers(Wrk, Peers) ->
    Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Time, Sleep, Jitter) ->
    Wait = random:uniform(Sleep),
    receive
        {msg, SentTime, Msg} ->
            NewTime = time:merge(Time, SentTime),
            Log ! {log, Name, NewTime, {received, Msg}},
            loop(Name, Log, Peers, NewTime, Sleep, Jitter);
        stop ->
            ok;
        Error ->
            Log ! {log, Name, time, {error, Error}}
        after Wait ->
            Selected = select(Peers),
            NewTime = time:inc(t, Time),
            Message = {hello, random:uniform(100)},
            Selected ! {msg, NewTime, Message},
            jitter(Jitter),
            Log ! {log, Name, NewTime, {sending, Message}},
            loop(Name, Log, Peers, NewTime, Sleep, Jitter)
    end.

select(Peers) ->
    lists:nth(random:uniform(length(Peers)), Peers).

jitter(0) -> ok;
jitter(Jitter) ->
    timer:sleep(random:uniform(Jitter)).

