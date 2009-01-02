-module (twitter_poller).
-behaviour(gen_server).
-include ("twitter.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, code_change/3, handle_info/2, terminate/2]).
-export([update/0, random_tweet/0, tweets/0]).

start_link() ->
  gen_server:start_link({local, twitter_poller}, twitter_poller, [], []).
  
update() ->
  gen_server:cast(twitter_poller, {update, request_tweets()}).
   
tweets() ->
  gen_server:call(twitter_poller, tweets).
  
random_tweet() ->
  gen_server:call(twitter_poller, random_tweet).

request_tweets() ->
  {ok, {_Status, _Headers, TwitterJson}} = http:request("http://search.twitter.com/search.json?q=%22i+hate%22"),
  % {ok, {obj, DecodedJson}, _Remainder} = rfc4627:decode(TwitterJson),
  {struct, DecodedJson} = mochijson2:decode(TwitterJson),
  [Results | _QueryMetaData] = DecodedJson,
  {<<"results">>, JsonTweets} = Results,
  Tweets = lists:map(
    fun(JsonTweet) -> 
      {struct,Fields} = JsonTweet,
      lists:foldl(fun(Field, Tweet) -> extract_field(Field, Tweet) end, #tweet{}, Fields)
    end, JsonTweets),
  io:format("got ~p tweets ~n", [length(Tweets)]),
  Tweets.

extract_field({<<"text">>, Text}, Tweet) ->
  Match = re:run(Text, "i hate(((http|www).*?(\\s|$))|.)*?(\\.|\\!|\\?|$)+", [{capture, first, binary}, caseless]),
  case Match of
    {match, FrancisQuote} ->
        Tweet#tweet{from_quote=Text, francis_quote=FrancisQuote};
    nomatch ->
      Tweet#tweet{from_quote=Text}
  end;
extract_field({<<"from_user">>, FromUser}, Tweet) ->
  Tweet#tweet{from=FromUser, from_url=list_to_binary("http://twitter.com/"++FromUser)};
extract_field({<<"profile_image_url">>, ImageUrl}, Tweet) ->
  Tweet#tweet{from_img=ImageUrl};
extract_field(_Field, Tweet) ->
  Tweet.
   
init(_Args) ->
  timer:apply_interval(60000, ?MODULE, update, []),
  {ok, request_tweets()}.
  
handle_cast({update, NewTweets}, _Tweets) ->
  FilteredTweets = lists:filter(fun(Tweet) -> Tweet#tweet.francis_quote =/= undefined end, NewTweets),
  {noreply, FilteredTweets}.
  
handle_call(tweets, _From, Tweets) ->
  {reply, Tweets, Tweets};
handle_call(random_tweet, _From, Tweets) ->
  TweetLength = length(Tweets),
  {reply, lists:nth(random:uniform(TweetLength), Tweets), Tweets}.
  
handle_info(_Info, State) ->
  {noreply, State}.
  
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
  
terminate(_Reason, _State) ->
  ok.