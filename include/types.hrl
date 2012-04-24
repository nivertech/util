%%%-------------------------------------------------------------------
%%% File    : types.hrl
%%% Created : 19/12/2010
%%%
%%% @doc type specs
%%%
%%% @author Zvi Avraham <zvi@nivertech.com>
%%% @copyright Nivertech (Nywhere Tech Ltd), 2010-2011
%%% @end
%%%-----------------------------------------------------------------------------

-ifndef(UTIL_TYPES_HRL).
-define(UTIL_TYPES_HRL, 1).


%% new proplist type introduced in R14B04
%-type proplist()   :: [{atom(),any()} | atom()].

-type qs_plist()    :: [{binary(),binary()}].

-type ipaddr()      :: {0..255,0..255,0..255,0..255}.
%-type ipaddr()     :: inet:ip_address().

-type tcp_port()    :: 0..65535.
-type tcp_endpoint():: {ipaddr(),tcp_port()}.

-type flood_plan()  :: [{node(), ipaddr(), non_neg_integer(), non_neg_integer()}].

-type mochi_request()   :: any().
-type mochi_wsrequest() :: any().
-type cowboy_request()  :: any().
-type mochi_response()  :: any().

-type json() :: {_}.

-type aggregate() :: statistic_set()|undefined.

-type metric() ::   {
                        atom(),  % name
                        {
                            number()|aggregate(),   % value
                            string(),
                            datetime()
                        }
                    }. % unit.

-ifndef(NO_ERLCLOUD_TYPES).
-type(datetime() :: {{pos_integer(), 1..12, 1..31}, {0..23, 0..59, 0..60}}).
%%------------------------------------------------------------------------------
%% @doc StatisticSet
%% @see[ http://docs.amazonwebservices.com/AmazonCloudWatch/latest/APIReference/index.html?API_StatisticSet.html ]
%% @end
%%------------------------------------------------------------------------------
-record(statistic_set, {
    sample_count    ::non_neg_integer(), %% The number of samples used for the statistic set.
    maximum         ::float(), %% The maximum value of the sample set.
    minimum         ::float(), %% The minimum value of the sample set.
    sum             ::float()  %% The sum of values for the sample set.
}).
-type statistic_set() :: #statistic_set{}.
-type(proplist() :: [{atom(), term()}|term()]).
-endif. %% NO_ERLCLOUD_TYPES


-endif. %% UTIL_TYPES_HRL

