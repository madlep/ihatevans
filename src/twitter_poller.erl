% Copyright (c) 2009 Julian Doherty
% 
% Permission is hereby granted, free of charge, to any person obtaining a copy
% of this software and associated documentation files (the "Software"), to deal
% in the Software without restriction, including without limitation the rights
% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
% copies of the Software, and to permit persons to whom the Software is
% furnished to do so, subject to the following conditions:
% 
% The above copyright notice and this permission notice shall be included in
% all copies or substantial portions of the Software.
% 
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
% THE SOFTWARE.

-module (twitter_poller).
-behaviour(gen_server).
-include ("twitter.hrl").

-define (TWITTER_URL, "http://search.twitter.com/search.json?q=%22i+hate%22").

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
  {ok, {_Status, _Headers, TwitterJson}} = http:request(get, {?TWITTER_URL, [{"User-Agent", "ihatevans.com twitter_poller"}]}, [], []),
  {struct, DecodedJson} = mochijson2:decode(TwitterJson),
  [Results | _QueryMetaData] = DecodedJson,
  {<<"results">>, JsonTweets} = Results,
  Tweets = lists:map(
    fun(JsonTweet) -> 
      {struct,Fields} = JsonTweet,
      lists:foldl(fun(Field, Tweet) -> extract_field(Field, Tweet) end, #tweet{}, Fields)
    end, JsonTweets),
  FilteredTweets = lists:filter(fun(Tweet) -> Tweet#tweet.francis_quote =/= undefined end, Tweets),
  JsonEncodedTweets = lists:map(
    fun(Tweet) ->
      JsonTerm = {struct, [
        {<<"francis_quote">>, Tweet#tweet.francis_quote},
        {<<"from">>,          Tweet#tweet.from},
        {<<"from_url">>,      Tweet#tweet.from_url},
        {<<"from_img">>,      Tweet#tweet.from_img},
        {<<"from_quote">>,    Tweet#tweet.from_quote}
      ]},
      mochijson2:encode(JsonTerm)
    end, FilteredTweets),
  io:format("got ~p tweets ~n", [length(JsonEncodedTweets)]),
  JsonEncodedTweets.

extract_field({<<"text">>, Text}, Tweet) ->
  Match = re:run(Text, "i hate(((http|www).*?(\\s|$))|.)*?(\\.|\\!|\\?|$)+", [{capture, first, binary}, caseless]),
  case Match of
    {match, FrancisQuote} ->
        Tweet#tweet{from_quote=Text, francis_quote=list_to_binary(FrancisQuote)};
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
  {noreply, NewTweets}.
  
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