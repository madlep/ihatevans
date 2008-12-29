%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(ihatevans).
-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the ihatevans server.
start() ->
    ihatevans_deps:ensure(),
    ensure_started(crypto),
    ensure_started(inets),
    application:start(ihatevans).

%% @spec stop() -> ok
%% @doc Stop the ihatevans server.
stop() ->
    Res = application:stop(ihatevans),
    application:stop(crypto),
    application:stop(inets),
    Res.
