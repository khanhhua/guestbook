-module(gb_www_sup).
-author("Khanh Hua").

-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Name = "gb_www",
  Func = {gb_www, start_link, []},
  RestartType = permanent,
  Shutdown = 5000,
  ChildType = worker,
  Mods = [gb_www],
  %% If ChildSpec is a tuple, it must have six elements
  WebServerSpec = {Name, Func, RestartType, Shutdown, ChildType, Mods},

  {ok, {{one_for_one, 5, 3600}, [WebServerSpec]}}.
