%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% File: util_rpc.erl
%% Date: 25/08/2010
%%
%% @doc RPC utilities
%% see: [http://levgem.livejournal.com/280954.html]
%%
%% @copyright 2010-2011 Nivertech (Nywhere Tech Ltd)
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(util_rpc).

-export([
         call/2,
         call/3,
         multicall/3
        ]).

-include_lib("include/types.hrl").
-include_lib("include/log.hrl").

%%------------------------------------------------------------------------------
%% @doc optimized gen_server:call with fixed timeout of 15 sec
%% see: [http://levgem.livejournal.com/280954.html]
%% see: [https://github.com/maxlapshin/gen_server_bm/blob/master/src/my_client2.erl]
%% @end
%%------------------------------------------------------------------------------
-spec call(Server::pid(), Event::term()) -> term()|none().
call(Server, Event) ->
    Ref = erlang:make_ref(),
    Server ! {'$gen_call', {self(), Ref}, Event},
    erlang:yield(),
    receive
        {Ref, Reply} -> Reply
    after
        15000 -> erlang:error(timeout_call)
    end.

%%------------------------------------------------------------------------------
%% @doc optimized gen_server:call with timeout
%% see: [http://levgem.livejournal.com/280954.html]
%% see: [https://github.com/maxlapshin/gen_server_bm/blob/master/src/my_client2.erl]
%% @end
%%------------------------------------------------------------------------------
-spec call(Server::pid(), Event::term(), TimeoutMS::non_neg_integer()|infinity) -> term()|none().
call(Server, Event, TimeoutMS) ->
    Ref = erlang:make_ref(),
    Server ! {'$gen_call', {self(), Ref}, Event},
    erlang:yield(),
    receive
        {Ref, Reply} -> Reply
    after
        TimeoutMS -> erlang:error(timeout_call)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Quick and dirty multicall code
%% If you need to make gen_server:call to 1000 processes, than utilize all 1000 cores of your Intel Core i19,
%% don't make 1000 sequential calls, just make one multicall
%%
%% A special intermediate process is created for collecting calls.
%% Some of them may fail due timeout. You are warned.
%% @end
%%------------------------------------------------------------------------------
-spec multicall(Pids::[pid()], What::term(), TimeoutMS::non_neg_integer()|infinity) -> [{pid(),term()}].%%TODO: | none().
multicall(Pids, What, TimeoutMS) ->
    Parent = self(),
    Caller = proc_lib:spawn(fun() -> multicall_helper(Parent, Pids, What) end),
    Ref = erlang:monitor(process, Caller),
    ok =    receive
                {multicall_ready, Caller} -> ok;
                {'DOWN', Ref, process, Caller, normal} -> ok;
                {'DOWN', Ref, process, Caller, Reason} -> {error, {caller_down, Reason}}
            after
                TimeoutMS ->  erlang:exit(Caller, shutdown), ok
            end,
    erlang:demonitor(Ref, [flush]),
    collect_multicall_replies(Pids, []).

collect_multicall_replies([], Acc) ->
    lists:reverse(Acc);

collect_multicall_replies([Pid|Pids], Acc) ->
    receive
        {multicall_reply, Pid, Reply} -> collect_multicall_replies(Pids, [{Pid,Reply}|Acc])
    after
        0 -> collect_multicall_replies(Pids, Acc)
    end.

multicall_helper(Parent, Pids, Request) ->
    erlang:monitor(process, Parent),
    Refs = send_multicalls(Pids, Request, []),
    collect_multicalls(Parent, Refs).

send_multicalls([], _Request, Refs) ->
    Refs;

send_multicalls([Pid|Pids], Request, Refs) ->
    Ref = make_ref(),
    erlang:send(Pid, {'$gen_call', {self(), Ref}, Request}),
    send_multicalls(Pids, Request, [{Ref,Pid}|Refs]).

collect_multicalls(Parent, []) ->
    Parent ! {multicall_ready, self()};

collect_multicalls(Parent, Refs) ->
    receive
        {'DOWN', _Ref, process, Parent, _Reason} -> ok;
        {Ref, Reply} ->
            Refs1 = case lists:keytake(Ref, 1, Refs) of
                        false -> Refs;
                        {value, {Ref, Pid}, Refs1_} ->
                            Parent ! {multicall_reply, Pid, Reply},
                            Refs1_
                    end,
            collect_multicalls(Parent, Refs1)
    end.

