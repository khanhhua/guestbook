-module(gb_www_sup).
-author("Khanh Hua").

-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  WebServerSpec = {gb_www, {gb_www, start_link, []}, permanent, 5000, worker},

  {ok, {{one_for_one, 5, 3600}, [WebServerSpec]}}.