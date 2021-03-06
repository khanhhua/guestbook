-module(gb_server).
-author("Khanh Hua").

-behavior(gen_server).

-export([init/1, 
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([start/0, stop/0, add_guest/1, list_guests/0, create_message/1,
  list_messages/0,
  list_messages/2]).

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
  gen_server:call(?SERVER, {create_message, Message}).

-spec(list_messages() -> {ok, [map()]}).
list_messages() ->
  gen_server:call(?SERVER, {list_messages}).

-spec(list_messages(Condition :: term(), Limit :: integer()) -> {ok, [map()]}).
list_messages(Condition, Limit) ->
  gen_server:call(?SERVER, {list_messages, Condition}).
%%% ==================================
%%% gen_server callbacks
%%% ==================================

init(_Args) ->
  HashidContext = hashids:new([{salt, "SALTYASSALT"}]),

  {ok, #{
    hashid => HashidContext,
    guests => [],
    messages => []
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
    {create_message, Message} ->
      {ok, UpdatedMessage, UpdatedState} = create_message(Message, State),
      {reply, {ok, UpdatedMessage}, UpdatedState}
    ;
    {list_messages} ->
      List = list_messages(State),
      {reply, {ok, List}, State}
    ;
    {list_messages, Condition} when is_function(Condition) ->
      List = list_messages(State),
      FilteredList = lists:filter(Condition, List),
      {reply, {ok, FilteredList}, State}
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

  case lists:filter(
    fun ({guest, _Id, GName, GContact}) ->
      Name =:= GName andalso Contact =:= GContact
    end,
    Guests) of
    [] ->
      Id = list_to_binary(hashids:encode(HashidContext, trunc(rand:uniform() * 1000000000))),
      UpdatedGuests = [{guest, Id, Name, Contact} | Guests],
      UpdatedState = State#{ guests := UpdatedGuests },
      {ok, {guest, Id, Name, Contact}, UpdatedState}
    ;
    [{guest, Id, Name, Contact}] ->
      {ok, {guest, Id, Name, Contact}, State}
  end.

list_guests(State) ->
  #{guests := Guests} = State,

  Guests.

create_message(Message, State) ->
  #{messages := Messages,
    hashid   := HashidContext} = State,
  {message, GuestId, Text} = Message,

  Id = list_to_binary(hashids:encode(HashidContext, trunc(rand:uniform() * 1000000000))),
  NewMessage = {message, Id, GuestId, Text, calendar:universal_time()},
  UpdatedMessages = [NewMessage | Messages],
  UpdatedState = State#{ messages := UpdatedMessages },
  {ok, NewMessage, UpdatedState}.

list_messages(State) ->
  #{guests   := Guests,
    messages := Messages} = State,

  lists:map(
    fun ({message, Id, GuestId, Text, CreatedAt}) ->
      {guest, GuestId, GuestName, _} = lists:keyfind(GuestId, 2, Guests),
      {message, Id, GuestId, GuestName, Text, CreatedAt}
    end,
    Messages).