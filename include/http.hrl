%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File: http.hrl
%%% Date: 19/04/2012
%%%
%%% @doc HTTP constants
%%%
%%% @author Zvi Avraham <zvi@nivertech.com>
%%% @copyright 2012 Nivertech (Nywhere Tech Ltd)
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifndef(UTIL_HTTP_HRL).
-define(UTIL_HTTP_HRL, 1).


%% Examples of REST API status codes:
%%  [http://pusher.com/docs/rest_api]
%% 202 ACCEPTED: The event has been received and will be send asynchronously to all sockets.
%% 400 BAD REQUEST: The request body will contain details.
%% 401 UNAUTHORIZED: Authentication failure - the response body will contain details.
%% 404 NOT FOUND: Resource not found - most likely appid does not exist.
%% 500 INTERNAL SERVER ERROR

%% HTTP status values
-define(HTTP_OK,                    200).
-define(HTTP_ACCEPTED,              202).
-define(HTTP_NO_CONTENT,            204).
-define(HTTP_MOVED_PERMANENTLY,     301).
-define(HTTP_FOUND,                 302).
-define(HTTP_SEE_OTHER,             303).
-define(HTTP_NOT_MODIFIED,          304).
-define(HTTP_TEMPORARY_REDIRECT,    307).
-define(HTTP_BAD_REQUEST,           400).
-define(HTTP_UNAUTHORIZED,          401).
-define(HTTP_FORBIDDEN,             403).
-define(HTTP_NOT_FOUND,             404).
-define(HTTP_METHOD_NOT_ALLOWED,    405).
-define(HTTP_REQUEST_TIMEOUT,       408).
-define(HTTP_CONFLICT,              409).
-define(HTTP_PRECONDITION_FAILED,   412).
-define(HTTP_INTERNAL_ERROR,        500).
-define(HTTP_SERVICE_UNAVAILIBLE,   503).


-endif. %% UTIL_HTTP_HRL

