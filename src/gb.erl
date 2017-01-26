%%% ====================================================
%%% Guestbook Application
%%% ====================================================

-module(gb).
-author("Khanh Hua").

-behavior(application).

-export([start/2, stop/1]).

%% application callbacks 

start(_Type, _Args) ->
  gb_server:start(),
  gb_www_sup:start_link().

stop(_State) ->
  ok.