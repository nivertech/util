%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% File: util.erl
%% Date: 25/08/2010
%%
%% @doc utility functions
%%
%% @author Zvi Avraham <zvi@nivertech.com>
%% @copyright 2010-2011 Nivertech (Nywhere Tech Ltd)
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(util).

-export([
         get_option/2,
         get_prm/2,
         get_prm_with_os_env/3,
         get_message_queue_lens/1,
         get_message_queue_len_values/1,
         is_process_alive/1,
		 is_numeric/1,
         is_int_str/1,
         get_random_pid/1,
         split/2,
         random_token/0,
         binary_to_hex/1,
         binary_to_hex_bin/1,
         to_s/1,
         mem_to_s/1,
         to_i/1,
         random_between/2,
         random_between_int/2,
         random_elem/1,
         is_guid/1,
         is_guid2/1,
         is_guid_with_slash/1,
         is_guid_with_slash2/1,
         is_guid_without_colon/1,
         is_guid_without_colon2/1,
         is_jsonp_method_ok/1,
         is_email/1,
         num_to_float/1,
         int_floor/1,
         int_ceil/1,
         eval_expr/1,
         upload_modules/2,
         json_field/2,
         json_field/3,
         equals_or_undefined/2,
         error_tup_to_undefined/1,
         riak_connect/0,
         get_key_value/2,           
         get_key_value/3,
         merge_proplists/2,
         powerset/1,
         list_intersect/2,
         format_utc_timestamp/0,
         format_datetime_sec/1,
         format_datetime_min/1,
         metric_aggregate/1,
         metric_time/1,
         metric_value/1,
         metric_unit/1,
         metric_name/1,
         metric_value_by_name/2,
         metric_aggregate_by_name/2,
         vm_stats/0,
         mem_stats/0,
         aggregate_init/0,
         aggregate_put/2,
         safe_relative_path/1,
         partition/2,
        %use_high_priority_if_needed/0,
         is_amazon_instance/0,
         get_amazon_instance_id/0,
         get_amazon_public_hostname/0,
         ip2str/1,
         str2ip/1,
         take/2,
         http_get/1,
         strip_binary_whitespace/2,
         remove_default_port/1
        ]).

-include_lib("include/types.hrl").
-include_lib("include/log.hrl").

%%------------------------------------------------------------------------------
%% @doc parse option
%% @end
%%------------------------------------------------------------------------------
-spec get_option(atom(), proplist()) -> {any(), proplist()}.
get_option(Option, Options) ->
    %{?GETP(Option, Options), proplists:delete(Option, Options)}.
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%%------------------------------------------------------------------------------
%% @doc get parameter from application .app file
%% @end
%%------------------------------------------------------------------------------
-spec get_prm(Application::atom(), Name::atom()) -> term().
get_prm(Application, Name)
    when    is_atom(Application), 
            is_atom(Name) 
    ->
    {ok,Val} = application:get_env(Application, Name),
    Val.

%%-----------------------------------------------------------------------------------
%% @doc evaluate an Erlang expression
%% @end
%%-----------------------------------------------------------------------------------
-spec eval_expr(S::string()) -> term().
eval_expr(S) ->
	{ok,Scanned,_} = erl_scan:string(S ++ "."),
	{ok,Parsed} = erl_parse:parse_exprs(Scanned),
	{value, Result, _NewBindings} = erl_eval:exprs(Parsed, erl_eval:new_bindings()),
	Result.
					   
%%-----------------------------------------------------------------------------------
%% @doc get parameter from application .app file or if not found, from OS environment
%% @end
%%-----------------------------------------------------------------------------------
-spec get_prm_with_os_env(atom(), atom(), string()) -> term().
get_prm_with_os_env(Application, Name, Env) ->
	case application:get_env(Application, Name) of
		{ok, Value} ->
			Value;
		undefined ->
			case os:getenv(Env) of
				false -> error;
				Value -> eval_expr(Value)
			end
	end.

%%------------------------------------------------------------------------------
%% @doc get message queue lengths for list of registered process names
%%      the return value is a property list {ProcessName, MsgQueuLength}
%% @end
%%------------------------------------------------------------------------------
-spec get_message_queue_lens(RegisteredNames::[atom()]) -> [{atom(),non_neg_integer()}].
get_message_queue_lens(RegisteredNames) ->
    [{Name, case whereis(Name) of
                undefined -> 0;
                P         -> case process_info(P, message_queue_len) of
                                 undefined              -> 0;
                                 {message_queue_len,Len}-> Len
                             end
            end}
     || Name <- RegisteredNames].

%%------------------------------------------------------------------------------
%% @doc get message queue lengths for list of registered process names
%%      the return value is a list of integers, 
%%      each representing message queue length of corresponding registered process
%% @end
%%------------------------------------------------------------------------------
-spec get_message_queue_len_values(RegisteredNames::[atom()]) -> [non_neg_integer()].
get_message_queue_len_values(RegisteredNames) ->
    [case process_info(whereis(Name), message_queue_len) of
         undefined              -> 0;
         {message_queue_len,Len}-> Len
     end
     || Name <- RegisteredNames].

%%------------------------------------------------------------------------------
%% @doc check if process is alive (even if it's on remote node)
%% @end
%%------------------------------------------------------------------------------
-spec is_process_alive(Pid::pid()) -> boolean().
is_process_alive(Pid)
  when is_pid(Pid) ->
    rpc:call(node(Pid), erlang, is_process_alive, [Pid]).

%%------------------------------------------------------------------------------
%% @doc get random process pid from the process group
%% @end
%%------------------------------------------------------------------------------
-spec get_random_pid(Name::term()) -> {ok, pid()} | {error, {no_process, term()}}.
get_random_pid(Name) ->
    L = case pg2:get_members(Name) of
            {error, _} ->
                timer:sleep(100),
                pg2:get_members(Name);
            Pids when is_list(Pids) ->
                Pids
        end,
    if 
        L == [] ->
            {error, {no_process, Name}};
        true ->
            {_,_,X} = erlang:now(),
            {ok, lists:nth((X rem length(L)) + 1, L)}
    end.

%%------------------------------------------------------------------------------
%% @doc split binary to two parts using separator string
%% @end
%%------------------------------------------------------------------------------
-spec split(Sep::string(), Bin::binary()) -> {ok, binary(), binary()} | {more, binary()}.
split(Sep, Bin) 
  when is_list(Sep),
       is_binary(Bin) ->
    case re:split(Bin, Sep, [{return, binary}, {parts, 2}]) of
        [Bin1, Bin2] when size(Bin1) < size(Bin) andalso size(Bin2) < size(Bin) ->
            
            {ok, Bin1, Bin2};
        [Bin1] ->
            {more, Bin1}
    end.

%%------------------------------------------------------------------------------
%% @doc generate random token string
%% @end
%%------------------------------------------------------------------------------
-spec random_token() -> string().
random_token() ->
    Term = term_to_binary({node(), make_ref()}),
    Digest = erlang:md5(Term),
    binary_to_hex(Digest).

%%------------------------------------------------------------------------------
%% @doc binary to hex string
%% @end
%%------------------------------------------------------------------------------
-spec binary_to_hex(Bin::binary()) -> string().
binary_to_hex(Bin) 
    when is_binary(Bin) ->
    [oct_to_hex(N) || <<N:4>> <= Bin].

%%------------------------------------------------------------------------------
%% @doc binary to hex binary
%% @end
%%------------------------------------------------------------------------------
-spec binary_to_hex_bin(Bin::binary()) -> binary().
binary_to_hex_bin(Bin) 
    when is_binary(Bin) ->
    << <<(oct_to_hex(N)):8>> || <<N:4>> <= Bin >>.

%%------------------------------------------------------------------------------
%% @doc binary to hex binary
%% @end
%%------------------------------------------------------------------------------
-spec oct_to_hex(0..15) -> char().

oct_to_hex(0) -> $0;
oct_to_hex(1) -> $1;
oct_to_hex(2) -> $2;
oct_to_hex(3) -> $3;
oct_to_hex(4) -> $4;
oct_to_hex(5) -> $5;
oct_to_hex(6) -> $6;
oct_to_hex(7) -> $7;
oct_to_hex(8) -> $8;
oct_to_hex(9) -> $9;
oct_to_hex(10) -> $a;
oct_to_hex(11) -> $b;
oct_to_hex(12) -> $c;
oct_to_hex(13) -> $d;
oct_to_hex(14) -> $e;
oct_to_hex(15) -> $f.

%%------------------------------------------------------------------------------
%% @doc convert any sensible input argument to string/list
%% @end
%%------------------------------------------------------------------------------
-spec to_s(L::string()|atom()|integer()|float()) -> string().

to_s(L) when is_list(L) ->
    L;
to_s(A) when is_atom(A) ->
    erlang:atom_to_list(A);
to_s(I) when is_integer(I) ->
    integer_to_list(I);
to_s(F) when is_float(F) ->
    float_to_list(F).

%%------------------------------------------------------------------------------
%% @doc convert memory in bytes to string with KB,MB,HB,etc.
%% @end
%%------------------------------------------------------------------------------
-spec mem_to_s(MemBytes::integer()) -> iolist().

mem_to_s(MemBytes) when MemBytes > 1024*1024*1024*1024 ->
    io_lib:format("~.3f TB", [MemBytes / (1024*1024*1024*1024)]);
mem_to_s(MemBytes) when MemBytes > 1024*1024*1024 ->
    io_lib:format("~.3f GB", [MemBytes / (1024*1024*1024)]);
mem_to_s(MemBytes) when MemBytes > 1024*1024 ->
    io_lib:format("~.3f MB", [MemBytes / (1024*1024)]);
mem_to_s(MemBytes) when MemBytes > 1024 ->
    io_lib:format("~.3f KB", [MemBytes / 1024]);
mem_to_s(MemBytes) ->
    io_lib:format("~b B", [MemBytes]).

%%------------------------------------------------------------------------------
%% @doc converts a list or a binary to an integer
%% @end
%%------------------------------------------------------------------------------
-spec to_i(L::string()|binary()) -> integer().
to_i(L) when is_list(L) ->
    list_to_integer(L);
to_i(B) when is_binary(B) ->
    list_to_integer(binary_to_list(B)).

%%------------------------------------------------------------------------------
%% @doc generate random float number from the range [A,B]
%% @end
%%------------------------------------------------------------------------------
-spec random_between(A::float(),B::float()) -> float().
random_between(A,B) ->
	A + random:uniform()*(B-A).

%%------------------------------------------------------------------------------
%% @doc generate random integer number from the range [A,B]
%% @end
%%------------------------------------------------------------------------------
-spec random_between_int(A::integer(),B::integer()) -> integer().
random_between_int(A,B) ->
	A + random:uniform(B-A) - 1.

%%------------------------------------------------------------------------------
%% @doc select a random element out of a list
%% @end
%%------------------------------------------------------------------------------
-spec random_elem(L::[X]) -> X.
random_elem(L) -> 
	lists:nth(random:uniform(length(L)), L).

%%------------------------------------------------------------------------------
%% @doc is GUID string, i.e. alphanumeric or '-' (dash) or ':' - minimum 1 character
%% for example:
%%  258EAFA5-E914-47DA-95CA-C5AB0DC85B11
%% @end
%%------------------------------------------------------------------------------
-spec is_guid(S::binary()) -> boolean().
is_guid(S) 
    when byte_size(S) >= 1 
    ->
    is_guid2(S);
is_guid(_) -> false.
 
%%------------------------------------------------------------------------------
%% @doc is GUID string, i.e. alphanumeric or '-' (dash) or ':' or '/' - minimum 1 character
%% for example:
%%  258EAFA5-E914-47DA-95CA-C5AB0DC85B11
%% @end
%%------------------------------------------------------------------------------
-spec is_guid_with_slash(S::binary()) -> boolean().
is_guid_with_slash(S) 
    when byte_size(S) >= 1 
    ->
    is_guid_with_slash2(S);
is_guid_with_slash(_) -> false.

%%------------------------------------------------------------------------------
%% @doc is GUID string, i.e. alphanumeric or '-' (dash) or ':'
%% for example:
%%  258EAFA5-E914-47DA-95CA-C5AB0DC85B11
%% @end
%%------------------------------------------------------------------------------
-spec is_guid2(S::binary()) -> boolean().
is_guid2(<<>>) ->
    true; 
is_guid2(<<Ch:8,Rest/bytes>>) 
    when 
        (($A=<Ch) andalso (Ch=<$Z)) orelse 
        (($a=<Ch) andalso (Ch=<$z)) orelse 
        (($0=<Ch) andalso (Ch=<$9)) orelse
         (Ch==$-) orelse
         (Ch==$:)
    -> 
    is_guid2(Rest); 
is_guid2(S) when is_binary(S) ->
    false. 

%%------------------------------------------------------------------------------
%% @doc is GUID string, i.e. alphanumeric or '-' (dash) or ':' or '/'
%% for example:
%%  258EAFA5-E914-47DA-95CA-C5AB0DC85B11
%% @end
%%------------------------------------------------------------------------------
-spec is_guid_with_slash2(S::binary()) -> boolean().
is_guid_with_slash2(<<>>) ->
    true; 
is_guid_with_slash2(<<Ch:8,Rest/bytes>>) 
    when 
        (($A=<Ch) andalso (Ch=<$Z)) orelse 
        (($a=<Ch) andalso (Ch=<$z)) orelse 
        (($0=<Ch) andalso (Ch=<$9)) orelse
         (Ch==$-) orelse
         (Ch==$:) orelse
         (Ch==$/)
    -> 
    is_guid_with_slash2(Rest); 
is_guid_with_slash2(S) when is_binary(S) ->
    false. 

%%------------------------------------------------------------------------------
%% @doc is GUID string, i.e. alphanumeric or '-' (dash) - minimum 1 character
%% for example:
%%  258EAFA5-E914-47DA-95CA-C5AB0DC85B11
%% @end
%%------------------------------------------------------------------------------
-spec is_guid_without_colon(S::binary()) -> boolean().
is_guid_without_colon(S) 
    when byte_size(S) >= 1 
    ->
    is_guid_without_colon2(S);
is_guid_without_colon(_) -> false.
 
%%------------------------------------------------------------------------------
%% @doc is GUID string, i.e. alphanumeric or '-' (dash)
%% for example:
%%  258EAFA5-E914-47DA-95CA-C5AB0DC85B11
%% @end
%%------------------------------------------------------------------------------
-spec is_guid_without_colon2(S::binary()) -> boolean().
is_guid_without_colon2(<<>>) ->
    true; 
is_guid_without_colon2(<<Ch:8,Rest/bytes>>) 
    when 
        (($A=<Ch) andalso (Ch=<$Z)) orelse 
        (($a=<Ch) andalso (Ch=<$z)) orelse 
        (($0=<Ch) andalso (Ch=<$9)) orelse
         (Ch==$-)
    -> 
    is_guid_without_colon2(Rest); 
is_guid_without_colon2(S) when is_binary(S) ->
    false. 

%%------------------------------------------------------------------------------
%% @doc is javascript identifier? i.e. alphanumeric or '_' (undercore), 
%%      not starting with digit
%% @end
%%------------------------------------------------------------------------------
-spec is_identifier(S::binary()) -> boolean().
is_identifier(<<Ch:8,_Rest/bytes>> = S) 
    when (($0>Ch) orelse (Ch>$9))
    ->
    is_identifier2(S);
is_identifier(_) -> false.

%%------------------------------------------------------------------------------
%% @doc is javascript identifier (also returns true for empty binary)
%% @end
%%------------------------------------------------------------------------------
-spec is_identifier2(S::binary()) -> boolean().
is_identifier2(<<>>) ->
    true; 
is_identifier2(<<Ch:8,Rest/bytes>>) 
    when  
        (($A=<Ch) andalso (Ch=<$Z)) orelse 
        (($a=<Ch) andalso (Ch=<$z)) orelse 
        (($0=<Ch) andalso (Ch=<$9)) orelse
         (Ch==$_)
    -> 
    is_identifier2(Rest); 
is_identifier2(S) when is_binary(S) ->
    false. 

%%------------------------------------------------------------------------------
%% @doc is letter?
%% @end
%%------------------------------------------------------------------------------
-spec is_alpha(Ch::byte()) -> boolean().
is_alpha(Ch) 
    when (($A=<Ch) andalso (Ch=<$Z)) orelse 
         (($a=<Ch) andalso (Ch=<$z)) 
    -> true;
is_alpha(_) -> false.
 
%%------------------------------------------------------------------------------
%% @doc is digit?
%% @end
%%------------------------------------------------------------------------------
-spec is_digit(Ch::byte()) -> boolean().
is_digit(Ch) 
    when (($0=<Ch) andalso (Ch=<$9))    
    -> true;
is_digit(_) -> false.

%%------------------------------------------------------------------------------
%% @doc is JSONP method valid?
%% @end
%%------------------------------------------------------------------------------
-spec is_jsonp_method_ok(JSONPMethod::binary()) -> boolean().
is_jsonp_method_ok(JSONPMethod) ->
    is_identifier2(JSONPMethod).

%%------------------------------------------------------------------------------
%% @doc is email valid?
%% @end
%%------------------------------------------------------------------------------
-spec is_email(Email::binary()) -> boolean().
is_email(Email) ->
    %% see http://www.regular-expressions.info/email.html
    re:run(Email, <<"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,4}\$">>) =/= nomatch. 

%%------------------------------------------------------------------------------
%% @doc convert numeric value to float
%% @end
%%------------------------------------------------------------------------------
-spec num_to_float(X::number()) -> float().
num_to_float(X) when is_integer(X) -> X + 0.0;
num_to_float(X) -> X.

%%------------------------------------------------------------------------------
%% @doc integer floor
%% @end
%%------------------------------------------------------------------------------
-spec int_floor(X::number()) -> integer(). 
int_floor(X) when is_integer(X)-> X;
int_floor(X) -> 
     T = trunc(X), 
     if X < T -> T - 1;
        true  -> T
     end.

%%------------------------------------------------------------------------------
%% @doc integer ceil
%% @end
%%------------------------------------------------------------------------------
-spec int_ceil(X::number()) -> integer(). 
int_ceil(X) when is_integer(X) -> X;
int_ceil(X) ->
    T = trunc(X),
    if X > T -> T + 1;
       true  -> T
    end.
            
%%------------------------------------------------------------------------------
%% @doc check if string is numeric
%% @end
%%------------------------------------------------------------------------------
-spec is_numeric(L::string()|binary()) -> boolean().
is_numeric(L) when is_list(L) ->
    try erlang:list_to_integer(L) of
        _ -> true
    catch
        _:_ ->
            try erlang:list_to_float(L) of
                _   -> true
            catch
                _:_ -> false
            end
    end;
is_numeric(B) when is_binary(B) ->
    case re:run(B, <<"^[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?\$">>) of
        {match, _} -> true;
        nomatch -> false
    end.

%%------------------------------------------------------------------------------
%% @doc check if string is integer
%% @end
%%------------------------------------------------------------------------------
-spec is_int_str(L::string()) -> boolean().
is_int_str(L) ->
    try erlang:list_to_integer(L) of
        _   -> true
    catch
        _:_ -> false
    end.

%%------------------------------------------------------------------------------
%% @doc uploads hooks to riak node while uploading auxiliary modules as well
%% @end
%%------------------------------------------------------------------------------
-spec upload_modules(Nodes::[atom()], ModuleList::[atom()]) -> ok.                            
upload_modules(Nodes, ModuleList) ->
    [begin
         {Mod, Bin, File} = code:get_object_code(ModuleName),
         {LBRes, []} = rpc:multicall(Nodes, code, load_binary, [Mod, File, Bin]),
         lists:foreach(fun({module,_}) -> ok end, LBRes)
     end 
     || ModuleName <- ModuleList],
    ok.

%%------------------------------------------------------------------------------
%% @doc extracts a field from JSON, if value is not in JSON, returns a default
%% @end
%%------------------------------------------------------------------------------
-spec json_field(Field::binary(), JSON::{any()}, Default::any()) -> any().
json_field(Field, JSON, Default) ->
    {Contents} = JSON,
    ?GETP(Field, Contents, Default).

-spec json_field(Field::binary(), JSON::{any()}) -> any().
json_field(Field, JSON) ->
    json_field(Field, JSON, undefined).

%%------------------------------------------------------------------------------
%% @doc checks if a value is X or undefined
%% @end
%%------------------------------------------------------------------------------
-spec equals_or_undefined(Value::any(), X::any()) -> boolean().
equals_or_undefined(Value, X) ->
    (Value == undefined) orelse (Value == X).

%%------------------------------------------------------------------------------
%% @doc If a value is equal to {error, _}, replaces it with undefined. 
%% Otherwise, does nothing.
%% @end
%%------------------------------------------------------------------------------
-spec error_tup_to_undefined(term()) -> term().
error_tup_to_undefined({error, _}) -> undefined;
error_tup_to_undefined(X) -> X.

%%------------------------------------------------------------------------------
%% @doc connects to one of the riak nodes
%% @end
%%------------------------------------------------------------------------------
-spec riak_connect() -> {ok, term()}|{error, term()}|no_riak.
riak_connect() ->
    RiaksFromConfig = config:riak_nodes(),
    case RiaksFromConfig of
        [] ->
            no_riak;
        _ ->
            RiaksWithKeys = [{random:uniform(), R} || R <- RiaksFromConfig],
            RiaksSorted = lists:keysort(1, RiaksWithKeys),
            Riaks = [R || {_,R} <- RiaksSorted],
            lists:foldl(fun(R, {error, _}) -> riak:client_connect(R);
                           (_, {ok, C})    -> {ok, C} end,
                        {error, dummy},
                        Riaks)
    end.

%%------------------------------------------------------------------------------
%% @doc faster than proplists:get_value
%% @end
%%------------------------------------------------------------------------------
-spec get_key_value(Key::term(), PL::[{term(), term()}]) -> undefined | term().
get_key_value(Key, PL) ->
	case lists:keyfind(Key, 1, PL) of
		false       -> undefined;
		{_K, Value} -> Value
	end.

%%------------------------------------------------------------------------------
%% @doc faster than proplists:get_value, but with default value
%% @end
%%------------------------------------------------------------------------------
-spec get_key_value(Key::term(), PL::[{term(), term()}], Default::term()) -> undefined | term().
get_key_value(Key, PL, Default) ->
	case lists:keyfind(Key, 1, PL) of
		false       -> Default;
		{_K, Value} -> Value
	end.

%%------------------------------------------------------------------------------
%% @doc like dict merge but on proplists
%% @end
%%------------------------------------------------------------------------------
-spec merge_proplists(proplist(), proplist()) -> proplist().
merge_proplists(Old, New) ->
    dict:to_list(dict:merge(fun (_Key, _OldV, NewV) -> NewV end, 
                            dict:from_list(Old), 
                            dict:from_list(New))).

%%------------------------------------------------------------------------------
%% @doc Calculate Powerset, i.e. all subsets except for empty set of elements
%% @see [http://en.wikipedia.org/wiki/Power_set]
%% @end
%%------------------------------------------------------------------------------
-spec powerset(list()) -> [list()].

powerset([])        -> [];
powerset([X])       -> [[X]];
powerset([X,Y])     -> 
    [
        [X  ],
        [  Y],
        [X,Y]
    ];
powerset([X,Y,Z])   -> 
    [
        [X    ],
        [  Y  ],
        [    Z],
        [X,Y  ],
        [X,  Z],
        [  Y,Z],
        [X,Y,Z]
    ];
powerset([X,Y,Z,W]) -> 
    [
        [X      ],
        [X,Y    ],
        [X,Y,Z  ],
        [X,Y,Z,W],
        [X,Y,  W],
        [X,  Z  ],
        [X,  Z,W],
        [X,    W],
        [  Y    ],
        [  Y,Z  ],
        [  Y,Z,W],
        [  Y,  W],
        [    Z  ],
        [    Z,W],
        [      W]
    ];
powerset([H|T]) ->
    P = powerset(T),
    [[H]] ++ [ [H|X] || X<-P ] ++ P.

%%------------------------------------------------------------------------------
%% @doc Calculate Powerset, i.e. all subsets except for empty set of elements
%% @see [http://en.wikipedia.org/wiki/Power_set]
%% @end
%%------------------------------------------------------------------------------
-spec list_intersect(list(), list()) -> list().
list_intersect(L1, L2) ->
    lists:filter(fun(X) -> lists:member(X, L1) end, L2).

%%------------------------------------------------------------------------------
%% @doc format UTC Date and Time timestamp (where ZZZ is miliseconds)
%%      YYYY-MM-DD HH:mm:SS.ZZZ
%% @end
%%------------------------------------------------------------------------------
-spec format_utc_timestamp() -> binary().
format_utc_timestamp() ->
    {_, _, Micro} = Now = os:timestamp(),
    DateTime = calendar:now_to_local_time(Now),
    {{Y, M, D}, {H, Mi, S}} = 
        case calendar:local_time_to_universal_time_dst(DateTime) of
            []     -> calendar:local_time();
            [T0|_] -> T0
        end,
    Ms = Micro div 1000 rem 1000,
    ?FMTB("~4..0b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b.~3..0b UTC", [Y, M, D, H, Mi, S, Ms]).

%%------------------------------------------------------------------------------
%% @doc format Date & time as a 1-sec key string
%%      YYYYMMDD_HHmmSS
%% @end
%%------------------------------------------------------------------------------
-spec format_datetime_sec(datetime()) -> binary().
format_datetime_sec({{Y,Mo,D},{H,Mi,S}}) ->
    ?FMTB("~4.10.0b~2.10.0b~2.10.0b_~2.10.0b~2.10.0b~2.10.0b", [Y,Mo,D,H,Mi,S]).

%%------------------------------------------------------------------------------
%% @doc format Date & time as a 1-min key string
%%      YYYYMMDD_HHmm
%% @end
%%------------------------------------------------------------------------------
-spec format_datetime_min(datetime()) -> binary().
format_datetime_min({{Y,Mo,D},{H,Mi,_}}) ->
    ?FMTB("~4.10.0b~2.10.0b~2.10.0b_~2.10.0b~2.10.0b", [Y,Mo,D,H,Mi]).

%%------------------------------------------------------------------------------
%% AWS utility functions
%%------------------------------------------------------------------------------

%% TODO : this will not work with short names
-spec is_amazon_instance() -> boolean().
is_amazon_instance() ->
    Toks = string:tokens(atom_to_list(node()), "."),
	case lists:nthtail(case length(Toks) - 2 of X when X > 0 -> X ; _ -> 0 end, Toks) of
		["compute-1","internal"] -> true;
		["ec2"      ,"internal"] -> true;
		_                        -> false
	end.

-spec get_amazon_instance_id() -> string()|undefined.
get_amazon_instance_id() ->
    case get('instance-id') of
        undefined ->
            Val = case http_get("http://169.254.169.254/latest/meta-data/instance-id") of
                    {ok,_,_,InstanceID} ->
                        InstanceID;
                    {error, E} ->
                        ?WARN("get_amazon_instance_id failed with error ~p~n", [E]),
                        undefined
                  end,
            put('instance-id',Val),
            Val;
        V -> 
            V
    end.

-spec get_amazon_public_hostname() -> string()|undefined.
get_amazon_public_hostname() ->
    case get('public-hostname') of
        undefined ->
            Val =   case http_get("http://169.254.169.254/latest/meta-data/public-hostname") of
                        {ok,_,_,PublicHostname} ->
                           PublicHostname;
                        {error, E} ->
                            ?WARN("get_amazon_public_hostname with error ~p~n", [E]),
                            undefined
                    end,
            put('public-hostname',Val),
            Val;
        V ->
            V
    end.

-spec get_amazon_region() -> string()|undefined.
get_amazon_region() ->
    case get('availability-zone') of
        undefined ->
             Val =  case http_get("http://169.254.169.254/latest/meta-data/placement/availability-zone") of
                        {ok,_,_,"us-east-1"++_} ->
                            "us-east-1";
                        {ok,_,_,"us-west-1"++_} ->
                            "us-west-1";
                        {ok,_,_,"us-west-2"++_} ->
                            "us-west-2";
                        {ok,_,_,"eu-west-1"++_} ->
                            "eu-west-1";
                        {ok,_,_,"ap-southeast-1"++_} ->
                            "ap-southeast-1";
                        {ok,_,_,"ap-northeast-1"++_} ->
                            "ap-northeast-1";
                        {ok,_,_,"sa-east-1"++_} ->
                            "sa-east-1";
                        {error,E} ->
                            ?WARN("get_amazon_region returned with error ~p~n", [E]),
                            undefined
                    end,
                put('availability-zone',Val),
                Val;
        V ->
            V
    end.

%%------------------------------------------------------------------------------
%% metric API functions
%%------------------------------------------------------------------------------

%% TODO - why not just use record to store metric? looks like too complicated

-spec metric_time(metric()) -> datetime().
metric_time({_, {_, _, T}}) -> T.
    

-spec metric_value(metric()) -> number().
metric_value({_, {V, _, _}}) when is_number(V) -> V;
metric_value({_, {V, _, _}}) -> undefined.

-spec metric_aggregate(metric()) -> aggregate().
metric_aggregate({_, {V, _, _}}) when is_number(V) -> undefined;
metric_aggregate({_, {V, _, _}}) -> V.
    
-spec metric_unit(metric()) -> string().
metric_unit({_, {_, U, _}}) -> U.

-spec metric_name(metric()) -> atom().
metric_name({N, _}) -> N.

-spec metric_value_by_name(atom(), [metric()]) -> number().
metric_value_by_name(N, L) ->
    metric_value(lists:keyfind(N, 1, L)).

-spec metric_aggregate_by_name(atom(), [metric()]) -> aggregate().
metric_aggregate_by_name(N, L) ->
    metric_aggregate(lists:keyfind(N, 1, L)).

-spec aggregate_init() -> aggregate().
aggregate_init() ->
    undefined.

-spec aggregate_put(number(), aggregate()) -> aggregate().
aggregate_put(Value, undefined) ->
    #statistic_set{
               minimum=Value,
               maximum=Value,
               sum=Value,
               sample_count=1
              };

aggregate_put(Value, #statistic_set{minimum=Min,maximum=Max,sum=Sum,sample_count=Cnt}) ->
    #statistic_set{
               minimum=min(Value, Min),
               maximum=max(Value, Max),
               sum=Sum+Value,
               sample_count=Cnt+1
              }.

%%------------------------------------------------------------------------------
%% Get VM metrics / statistics
%%------------------------------------------------------------------------------

-spec vm_stats() -> [metric()].
vm_stats() ->
    DT = calendar:universal_time(),
    Mem = erlang:memory(),
    [{process_count, {erlang:system_info(process_count), "Count", DT}} |
     [{list_to_atom(atom_to_list(A) ++ "_mem"), {?GETP(A, Mem), "Bytes", DT}} || 
         A <- proplists:get_keys(Mem)]].

-spec mem_stats() -> [metric()].
mem_stats() ->
    DT = calendar:universal_time(),
    MemPL = memsup:get_system_memory_data(),
    Names = [{free_swap_mem,    free_swap       },
             {cached_mem,       cached_memory   },
             {buffered_mem,     buffered_memory },
             {free_mem,         free_memory     }],
    [{A1, {?GETP(A2, MemPL), "Bytes", DT}} || {A1,A2} <- Names]. 


%%------------------------------------------------------------------------------
%% @doc Return the reduced version of a relative path or undefined if it
%%      is not safe. safe relative paths can be joined with an absolute path
%%      and will result in a subdirectory of the absolute path.
%% @end
%%------------------------------------------------------------------------------
-spec safe_relative_path(string()) -> string() | undefined.

safe_relative_path("/" ++ _) ->
    undefined;
safe_relative_path(P) ->
    safe_relative_path(P, []).

safe_relative_path("", Acc) ->
    case Acc of
        [] ->
            "";
        _ ->
            string:join(lists:reverse(Acc), "/")
    end;
safe_relative_path(P, Acc) ->
    case partition(P, "/") of
        {"", "/", _} ->
            %% /foo or foo//bar
            undefined;
        {"..", _, _} when Acc =:= [] ->
            undefined;
        {"..", _, Rest} ->
            safe_relative_path(Rest, tl(Acc));
        {Part, "/", ""} ->
            safe_relative_path("", ["", Part | Acc]);
        {Part, _, Rest} ->
            safe_relative_path(Rest, [Part | Acc])
    end.

%%------------------------------------------------------------------------------
%% @doc Inspired by Python 2.5's str.partition:
%%      partition("foo/bar", "/") = {"foo", "/", "bar"},
%%      partition("foo", "/") = {"foo", "", ""}.
%% @end
%%------------------------------------------------------------------------------
-spec partition(String::string(), Sep::string()) -> {string(), string(), string()}.

partition(String, Sep) ->
    case partition(String, Sep, []) of
        undefined ->
            {String, "", ""};
        Result ->
            Result
    end.

partition("", _Sep, _Acc) ->
    undefined;
partition(S, Sep, Acc) ->
    case partition2(S, Sep) of
        undefined ->
            [C | Rest] = S,
            partition(Rest, Sep, [C | Acc]);
        Rest ->
            {lists:reverse(Acc), Sep, Rest}
    end.

partition2(Rest, "") ->
    Rest;
partition2([C | R1], [C | R2]) ->
    partition2(R1, R2);
partition2(_S, _Sep) ->
    undefined.

%%------------------------------------------------------------------------------
%% @doc bump current process' priority to high if needed
%% @end
%%------------------------------------------------------------------------------
-ifdef(UNCOMMENTED).
-spec use_high_priority_if_needed() -> ok.
use_high_priority_if_needed() ->
    case config_dynamic:use_high_priority() of
        true ->
            erlang:process_flag(priority, high),
            ok;
        false ->
            ok
    end.
-endif.

%% @doc convert IP address tuple to string, if string is given, it is unchanged
%% @see util:str2ip/1
-spec ip2str(Str::string()|ipaddr()) -> string().
ip2str(Str)
    when is_list(Str) ->
    Str;
ip2str({A4,A3,A2,A1}) ->
    ?FMT("~b.~b.~b.~b",[A4,A3,A2,A1]).

%% @doc convert string to IP address tuple, returns need_dns if
%%      string does not contain an IP address but a URL
%% @see util:ip2str/1
-spec str2ip(Str::string()) -> ipaddr()|need_dns.
str2ip(IPStr) ->
	X = (catch list_to_tuple( [ list_to_integer(L) || L <- string:tokens(IPStr,".") ] )),
	IsByte = fun(Y) -> is_integer(Y) and (0 =< Y) and (Y =< 255) end,
	case X of 
		{'EXIT',_} -> need_dns;
		{A,B,C,D} ->
			case IsByte(A) and IsByte(B) and IsByte(C) and IsByte(D) of
				true -> X;
				false -> need_dns
			end
	end.
			 
%% @doc takes first N elements of a list
-spec take(non_neg_integer(), list()) -> list().
take(N, L) -> 
    {FirstN, _} = lists:split(N, L),
    FirstN.

%% @doc simple http get
-spec http_get(string()) -> {ok, string(), [{string(), string()}], string()}|{error, term()}.
http_get(URL) ->
    ibrowse:send_req(URL, [], get).

%% @doc strips whitespaces from a binary (similar to string:strip)
-spec strip_binary_whitespace(binary(), left|right|both) -> binary().
strip_binary_whitespace(Bin, both) ->
    re:replace(Bin, <<"^\\s+|\\s+\$">>, "", [{return, binary}, global]);
strip_binary_whitespace(Bin, left) ->
    re:replace(Bin, <<"^\\s+">>, "", [{return, binary}]);
strip_binary_whitespace(Bin, right) ->
    re:replace(Bin, <<"\\s+\$">>, "", [{return, binary}]).

%% @doc removes port from URI in case it is default port for URI schema (currently supports: HTTP/80, HTTPS/443)
-spec remove_default_port(URI::binary()|string()) -> binary().
remove_default_port(URI) when is_list(URI) ->
    remove_default_port(list_to_binary(URI));
remove_default_port(<<"http://", _/bytes>>=URI) ->
    re:replace(URI, <<"http://([^/]+):80(/.*)?\$">>, <<"http://\\1\\2">>, [{return, binary}]);
remove_default_port(<<"https://", _/bytes>>=URI) ->
    re:replace(URI, <<"https://([^/]+):443(/.*)?\$">>, <<"https://\\1\\2">>, [{return, binary}]);
remove_default_port(URI) ->
    URI.

