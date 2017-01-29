-module(gb_server).
-author("Khanh Hua").

-behavior(gen_server).

-export([init/1, 
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([start/0, stop/0, add_guest/1, list_guests/0, create_message/1, fetch_messages/0]).

-define(SERVER, ?MODULE).

%%% ==================================
%%% Public interface
%%% ==================================

start() ->
  io:format("Starting guestbook internal server..."),
  gen_server:start({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_server:stop(?SERVER).

add_guest(Guest) ->
  gen_server:call(?SERVER, {add, Guest}).

list_guests() ->
  gen_server:call(?SERVER, {list}).

create_message(Message) ->
  gen_server:call(?SERVER, {message, Message}).

fetch_messages() ->
  gen_server:call(?SERVER, {fetch_messages}).
%%% ==================================
%%% gen_server callbacks
%%% ==================================

init(_Args) ->
  HashidContext = hashids:new([{salt, "SALTYASSALT"}]),

  {ok, #{
    hashid => HashidContext,
    guests => []
  }}.

handle_call(Req, From, State) ->
  io:format("[handle_call] Handling request: ~w~n", [Req]),
  
  case Req of
    {add, Guest} ->
      {ok, UpdatedGuest, UpdatedState} = add_guest(Guest, State),
      {reply, {ok, UpdatedGuest}, UpdatedState}
    ;
    {list} ->
      List = list_guests(State),
      {reply, {ok, List}, State}
    ;
    _ -> {reply, not_implemented, State}
  end.

handle_cast(Req, State) ->
  erlang:display("[handle_cast]"),
  erlang:display(Req),
  {noreply, State}.

handle_info(Req, State) ->
  {noreply, State}.

terminate(Reason, State) ->
  ok.

code_change(OldVsn, State, Extra) ->
  {ok, State}.

%%% ===================================
%%% Internal functions
%%% ===================================

add_guest(Guest, State) ->
  #{guests := Guests,
    hashid := HashidContext} = State,
  {guest, Name, Contact} = Guest,

  Id = list_to_binary(hashids:encode(HashidContext, trunc(rand:uniform() * 1000000000))),

  UpdatedGuests = [{guest, Id, Name, Contact} | Guests],
  UpdatedState = State#{ guests := UpdatedGuests },
  {ok, {guest, Id, Name, Contact}, UpdatedState}.

list_guests(State) ->
  #{guests := Guests} = State,

  Guests.
