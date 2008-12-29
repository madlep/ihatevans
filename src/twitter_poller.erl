-module (twitter_poller).

-include ("twitter.hrl").

-export ([start/0, update/0, tweets/0, loop/1, request_tweets/0, extract_field/2, random_tweet/0, loop_start/0]).

start() ->
  Pid = spawn(?MODULE, loop_start, []),
  register(twitter_poller, Pid),
  update().
  
update() ->
  twitter_poller ! {set_tweets, request_tweets()},
  ok.
   
tweets() ->
  twitter_poller ! {get_tweets, self()},
  receive
    {tweets, Tweets} ->
      Tweets
  end.
  
random_tweet() ->
  Tweets = tweets(),
  TweetLength = length(Tweets),
  lists:nth(random:uniform(TweetLength), Tweets).

request_tweets() ->
  {ok, {_Status, _Headers, TwitterJson}} = http:request("http://search.twitter.com/search.json?q=%22i+hate%22"),
  {ok, {obj, DecodedJson}, _Remainder} = rfc4627:decode(TwitterJson),
  [Results, _SinceId, _MaxId, _RefreshUrl, _ResultsPerPage, _NextPage, _CompletedIn, _Page, _Query] = DecodedJson,
  {"results", JsonTweets} = Results,
  Tweets = lists:map(
    fun(JsonTweet) -> 
      {obj,Fields} = JsonTweet,
      lists:foldl(fun(Field, Tweet) -> extract_field(Field, Tweet) end, #tweet{}, Fields)
    end, JsonTweets),
  io:format("got ~p tweets ~n", [length(Tweets)]),
  Tweets.

extract_field({"text", Text}, Tweet) ->
  Match = re:run(Text, "i hate(((http|www).*?(\\s|$))|.)*?(\\.|\\!|\\?|$)+", [{capture, first, binary}, caseless]),
  case Match of
    {match, FrancisQuote} ->
        Tweet#tweet{from_quote=Text, francis_quote=FrancisQuote};
    nomatch ->
      Tweet#tweet{from_quote=Text}
  end;
extract_field({"from_user", FromUser}, Tweet) ->
  Tweet#tweet{from=FromUser, from_url=list_to_binary("http://twitter.com/"++FromUser)};
extract_field({"profile_image_url", ImageUrl}, Tweet) ->
  Tweet#tweet{from_img=ImageUrl};
extract_field(_Field, Tweet) ->
  Tweet.
   
loop_start() ->
  timer:apply_interval(60000, ?MODULE, update, []),
  loop([request_tweets()]).
  
loop(Tweets) ->
  receive
    {set_tweets, NewTweets} ->
      loop(NewTweets);
    {get_tweets, From} ->
      From ! {tweets, Tweets},
      loop(Tweets);
    Any -> 
      io:format("Received ~p~n", [Any]),
      loop(Tweets)
  end.