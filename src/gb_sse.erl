%%%-------------------------------------------------------------------
%%% @author khanhhua
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Jan 2017 15:08
%%%-------------------------------------------------------------------
-module(gb_sse).
-author("khanhhua").

%% API
-export([init/3, handle/2, terminate/3]).

init(Protocol, Req, Opts) ->
  {ok, Req, no_state}.

handle(Req, State) ->
  io:format("[handle] Proc ~w receiving SSE ...~n", [self()]),
  Now = calendar:universal_time(),
  ets:insert(sse_clients, {pid_to_list(self()), Now}), %% { pid(), TimeTuple }

  Headers = [{<<"content-type">>, <<"text/event-stream">>}],
  {ok, Req2} = cowboy_req:chunked_reply(200, Headers, Req),

  Chunk = create_chunked_update(Now),
  erlang:display(Chunk),
  cowboy_req:chunk(Chunk, Req2),

  broadcast_presence(),

  handle_loop(Req2, #{now => Now}).

handle_loop(Req, State) ->
  receive
    shutdown -> {ok, Req, State}
    ;
    {presence, Count} ->
      io:format("[handle_loop] Updating connected sse with presence count ~w~n", [Count]),

      PresenceChunk = create_presence_chunk(Count),
      cowboy_req:chunk(PresenceChunk, Req),

      handle_loop(Req, State)
    ;
    {update, ReferenceTime} ->
      io:format("[handle_loop] Updating connected sse with reference time ~w~n", [ReferenceTime]),

      Chunk = create_chunked_update(ReferenceTime),
      cowboy_req:chunk(Chunk, Req),

      handle_loop(Req, State#{ now := ReferenceTime})
  after 5000 ->
    case heartbeat(Req) of
      ok -> handle_loop(Req, State)
      ;
      {error, Reason} ->
        io:format("[handle_loop] Shutting down due to client ~w~n", [Reason]),
        {ok, Req, State}
    end
  end.

terminate(Reason, Req, State) ->
  io:format("[terminate] Proc ~w has terminated ~n", [self()]),

  Pid = pid_to_list(self()),
  catch ets:match_delete(sse_clients, {Pid, '_'}),
  broadcast_presence(),

  ok.

%% Internal
create_chunked_update(ReferenceTime) ->
  {ok, Messages} = gb_server:list_messages(
    fun ({message, _Id, _GuestId, _GuestName, Text, CreatedAt}) ->
      io:format("[create_chunked_update] Now:~w Text:~s  At:~w~n", [ReferenceTime, Text, CreatedAt]),
      case calendar:time_difference(CreatedAt, ReferenceTime) of
        {0, {0,0,0}} -> true
        ;
        {Dd, _} when Dd < 0 -> true
        ;
        _ -> false
      end
    end,
    100
  ),
  List = lists:map(
    fun ({message, Id, GuestId, GuestName, Text, CreatedAt}) ->
      #{id        => Id,
        guest_id  => GuestId,
        guest_name=> GuestName,
        text      => Text,
        created_at=> CreatedAt}
    end,
    Messages),
  Data = jsx:encode(List),
  ["event: change\n", "data: ", Data, "\n\n"].

create_presence_chunk(Count) ->
  io:format("[create_presence_chunk] Presence count ~w~n", [Count]),

  ["event: presence\n", "data: ", integer_to_binary(Count), "\n\n"].

heartbeat(Req) ->
  io:format("Heartbeat...~n"),
  Chunk = ["event: heartbeat\n", "data: ok\n", "\n"],
  cowboy_req:chunk(Chunk, Req).

broadcast_presence() ->
  ClientProcessList = ets:tab2list(sse_clients),
  Count = length(ClientProcessList),
  lists:foreach(
    fun ({ClientPid, _}) ->
      Pid = list_to_pid(ClientPid),
      Pid! {presence, Count}
    end,
    ClientProcessList),
  ok.