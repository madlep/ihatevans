%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for ihatevans.

-module(ihatevans_web).
-author('author <author@example.com>').

-include ("twitter.hrl").

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->    
  {DocRoot, Options1} = get_option(docroot, Options),
  Loop = fun (Req) ->
    ?MODULE:loop(Req, DocRoot)
  end,
  mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
  mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
  "/" ++ Path = Req:get(path),
  case Req:get(method) of 
    Method when Method =:= 'GET'; Method =:= 'HEAD' ->
      dispatch(Req, Path, DocRoot);
    'POST' ->
      Req:not_found();
    _ ->
      Req:respond({501, [], []})
    end.

%% Internal API

%% send JSON update
dispatch(Req, "update.json", _DocRoot) ->
  Tweet = twitter_poller:random_tweet(),
  JsonTerm = {struct, [
    {<<"francis_quote">>, list_to_binary(Tweet#tweet.francis_quote)},
    {<<"from">>,          Tweet#tweet.from},
    {<<"from_url">>,      Tweet#tweet.from_url},
    {<<"from_img">>,      Tweet#tweet.from_img},
    {<<"from_quote">>,    Tweet#tweet.from_quote}
  ]},
  EncodedJson = mochijson2:encode(JsonTerm),
  Req:ok({"application/x-javascript; charset=utf-8", EncodedJson});

%% send a static file
dispatch(Req, Path, DocRoot) ->
  Req:serve_file(Path, DocRoot).

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
