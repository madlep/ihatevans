%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the ihatevans application.

-module(ihatevans_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for ihatevans.
start(_Type, _StartArgs) ->
    ihatevans_deps:ensure(),
    ihatevans_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for ihatevans.
stop(_State) ->
    ok.
