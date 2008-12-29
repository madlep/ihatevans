%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for ihatevans.

-module(ihatevans_web).
-author('author <author@example.com>').

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    ViewDir = "priv/views",
    {ok, Views} = file:list_dir(ViewDir),
    lists:foreach(
      fun(FileName) -> 
        erltl:compile(filename:join(ViewDir, FileName)) 
      end, Views),
    
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    % io:format("Path:~p~n", [Path]),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Path of
                "" ->
                  % erltl:compile("priv/views/index.html.et"),
                  Req:ok({"text/html", list_to_binary(index.html:render(twitter_poller:random_tweet()))});
                "update.json" ->
                  % erltl:compile("priv/views/update.json.et"),
                  Req:ok({"	application/x-javascript; charset=utf-8", list_to_binary(update.json:render(twitter_poller:random_tweet()))});
                _ ->
                    Req:serve_file(Path, DocRoot)
            end;
        'POST' ->
            case Path of
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
