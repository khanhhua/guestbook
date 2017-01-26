-module(gb_www).
-author("Khanh Hua").

-export([start_link/0]).

start_link() ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", cowboy_static, {priv_file, gb, "www/index.html"}},
      {"/[...]", cowboy_static, {priv_dir, gb, "www", [
        {mimetypes, cow_mimetypes, all},
        {dir_handler, directory_handler}
      ]}}
    ]}
  ]),
  {ok, Pid} = cowboy:start_http(my_http_listener, 100, [{port, 8080}], 
    [
      {env, [{dispatch, Dispatch}]}
      %, {middlewares, [cowboy_router, cowboy_handler]}
    ]),

  {ok, Pid}.