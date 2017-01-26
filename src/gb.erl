%%% ====================================================
%%% Guestbook Application
%%% ====================================================

-module(gb).
-author("Khanh Hua").

-behavior(application).

-export([start/2, stop/1]).

%% application callbacks 

start(_Type, _Args) ->
  gb_server:start().

stop(_State) ->
  ok.