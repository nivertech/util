%%%-------------------------------------------------------------------
%%% File    : log.hrl
%%% Created : 05/07/2010
%%%
%%% @doc Log macros to wrap log4erl calls and some misc macros
%%%
%%% @author Zvi Avraham <zvi@nivertech.com>
%%% @copyright Nivertech (Nywhere Tech Ltd), 2009-2010
%%% @end
%%%-----------------------------------------------------------------------------

-ifndef(LOG_HRL).
-define(LOG_HRL, true).

%-define(USE_LOG4ERL).
-undef(USE_LOG4ERL).

% some useful macros
-define(IF(Condition,A,B), 
        case (Condition) of 
            true    -> (A);
            false   -> (B)
        end
).

-define(IF(Condition,A),
        case (Condition) of
            true  -> (A);
            false -> ok
        end).

-define(STRIP_OK(X),
        (case (X) of
             {ok, StripOKHiddenVariable} -> StripOKHiddenVariable
         end)).

% get key value from property list
-define(GETP(Key, PL),          util:get_key_value(Key, PL) ).
-define(GETP(Key, PL, Default), util:get_key_value(Key, PL, Default) ).

-define(GETP_BOOL(Key, PL), 
        case util:get_key_value(Key, PL) of 
            false       -> false;
            true        -> true
        end
).
-define(GETP_BOOL(Key, PL, Default), 
        case util:get_key_value(Key, PL) of 
            false       -> false;
            true        -> true;
            undefined   -> Default 
        end
).

% format to list and flatten
-define( FMT(Fmt,Args), lists:flatten(io_lib:format(Fmt,Args)) ).
% format to binary
-define( FMTB(Fmt,Args), iolist_to_binary(io_lib:format(Fmt,Args)) ).
% format to iolist
%% -define( FMT_(Fmt,Args), io_lib:format(Fmt,Args) ).

% get current function M:F/A as list [M,F,A]
-define( CURRENT_FUNCTION, tuple_to_list(element(2,process_info(self(), current_function))) ).

%-define( LOG_FMT_ARGS(Args), begin {M,F,A} = element(2,process_info(self(), current_function)), [node(),M,F,A,?LINE | Args] end ).
-define( LOG_FMT_ARGS(Args), [node()] ++ ?CURRENT_FUNCTION ++ [?LINE | Args] ).

%TODO - how to specify logger ?
%-export([log/2, log/3, log/4]).
%-export([warn/1, warn/2, warn/3]).
%-export([info/1, info/2, info/3]).
%-export([error/1, error/2, error/3]).
%-export([fatal/1, fatal/2, fatal/3]).
%-export([debug/1, debug/2, debug/3]).

-define(INFO (Fmt),?INFO (Fmt,[])).
-define(WARN (Fmt),?WARN (Fmt,[])).
-define(DBG  (Fmt),?DBG  (Fmt,[])).
-define(ERROR(Fmt),?ERROR(Fmt,[])).
-define(FATAL(Fmt),?FATAL(Fmt,[])).

-ifdef(USE_LOG4ERL).    
%    -define(INFO( Fmt, Args), log4erl:info( "~p ~p:~p/~p :~p: " ++ Fmt, [node()] ++ ?CURRENT_FUNCTION ++ [?LINE | Args])).
%    -define(DBG(  Fmt, Args), log4erl:debug("~p ~p:~p/~p :~p: " ++ Fmt, [node()] ++ ?CURRENT_FUNCTION ++ [?LINE | Args])).
%    -define(WARN( Fmt, Args), log4erl:warn( "~p ~p:~p/~p :~p: " ++ Fmt, [node()] ++ ?CURRENT_FUNCTION ++ [?LINE | Args])).
%    -define(ERROR(Fmt, Args), log4erl:error("~p ~p:~p/~p :~p: " ++ Fmt, [node()] ++ ?CURRENT_FUNCTION ++ [?LINE | Args])).
%    -define(FATAL(Fmt, Args), log4erl:fatal("~p ~p:~p/~p :~p: " ++ Fmt, [node()] ++ ?CURRENT_FUNCTION ++ [?LINE | Args])).

    -define(INFO( Fmt, Args), log4erl:info( "~p ~p:~p/~p :~p: " ++ Fmt, ?LOG_FMT_ARGS(Args))).
    -define(DBG(  Fmt, Args), log4erl:debug("~p ~p:~p/~p :~p: " ++ Fmt, ?LOG_FMT_ARGS(Args))).
    -define(WARN( Fmt, Args), log4erl:warn( "~p ~p:~p/~p :~p: " ++ Fmt, ?LOG_FMT_ARGS(Args))).
    -define(ERROR(Fmt, Args), log4erl:error("~p ~p:~p/~p :~p: " ++ Fmt, ?LOG_FMT_ARGS(Args))).
    -define(FATAL(Fmt, Args), log4erl:fatal("~p ~p:~p/~p :~p: " ++ Fmt, ?LOG_FMT_ARGS(Args))).
-else.
%    -define(INFO( Fmt, Args), error_logger:info_msg("~p ~p:~p/~p :~p: " ++ Fmt, [node()] ++ ?CURRENT_FUNCTION ++ [?LINE | Args])).
    -define(INFO( Fmt, Args), error_logger:info_msg   ("~p ~p:~p/~p :~p: " ++ Fmt,  ?LOG_FMT_ARGS(Args))).
    %-define(DBG(  Fmt, Args), io:format               ("~p ~p:~p/~p :~p: " ++ Fmt,  ?LOG_FMT_ARGS(Args))).
    -define(DBG(  Fmt, Args), ok).

    -define(WARN( Fmt, Args), error_logger:warning_msg("~p ~p:~p/~p :~p: " ++ Fmt,  ?LOG_FMT_ARGS(Args))).
    -define(ERROR(Fmt, Args), error_logger:error_msg  ("~p ~p:~p/~p :~p: " ++ Fmt,  ?LOG_FMT_ARGS(Args))).
    -define(FATAL(Fmt, Args), todo).%TODO
-endif.

-endif. %% LOG_HRL

