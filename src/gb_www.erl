-module(gb_www).
-author("Khanh Hua").

-export([start_link/0]).

-export([init/3, handle/2, terminate/3]).
-export([
    query_guests/1,
    get_guest/2,
    create_guest/1,
    query_messages/1,
    get_message/2,
    create_message/1
]).

start_link() ->
  sse_clients = ets:new('sse_clients', [bag, public, named_table]),

  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", cowboy_static, {priv_file, gb, "www/index.html"}},
      {"/sse", gb_sse, []},
      {"/rest/guests/[:id]", ?MODULE, [{resource, guests}]},
      {"/rest/messages/[:id]", ?MODULE, [{resource, messages}]},
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

init(Protocol, Req, Opts) ->
  Resource = proplists:get_value(resource, Opts),
  {Id, Req} = cowboy_req:binding(id, Req, undefined),
  {Method, Req} = cowboy_req:method(Req),

  if
    Method =:= <<"GET">> ->
      case Resource of
        guests -> if
                    Id =/= undefined -> {ok, Req, {Resource, get_guest, [Req, Id]}}
                    ;
                    true -> {ok, Req, {Resource, query_guests, [Req]}}
                  end
        ;
        messages -> if
                      Id =/= undefined -> {ok, Req, {Resource, get_message, [Req, Id]}}
                      ;
                      true -> {ok, Req, {Resource, query_messages, [Req]}}
                    end
      end
    ;
    Method =:= <<"POST">> ->
      case Resource of
        guests -> {ok, Req, {Resource, create_guest, [Req]}}
        ;
        messages -> {ok, Req, {Resource, create_message, [Req]}}
      end
  end.

handle(Req, {Resource, Action, Args}) ->
  {ok, Req2} = case apply(?MODULE, Action, Args) of
    {ok, Body} ->
      erlang:display(Body),
      cowboy_req:reply(200,
        [
          {<<"content-type">>, <<"application/json">>}
        ],
        Body,
        Req)
    ;
    {error, Reason} ->
      cowboy_req:reply(400,
        [
          {<<"content-type">>, <<"application/json">>}
        ],
        Reason,
        Req)
  end,

  {ok, Req2, no_state}.

terminate(_Reason, Req, State) -> ok.

-spec(query_guests(Req::map()) -> {ok, [Guest::map()]} | {error, Reason::list()}).
query_guests(Req) ->
  {ok, Guests} = gb_server:list_guests(),
  List = lists:map(
    fun ({guest, Id, Name, Contact}) ->
      #{
        id => Id,
        name => Name,
        contact => Contact
      }
    end,
    Guests
  ),
  {ok, jsx:encode(List)}.

get_guest(Id, Req) ->
  {ok, <<"ok">>}.

create_guest(Req) ->
  {ok, Data, _} = cowboy_req:body(Req),
  Guest = jsx:decode(Data, [return_maps]),
  #{
    <<"name">> := Name,
    <<"contact">> := Contact
  } = Guest,

  {ok, {guest, Id, Name, Contact}} = gb_server:add_guest({guest, Name, Contact}),

  {ok, jsx:encode(#{
    id => Id,
    name => Name,
    contact => Contact
  })}.

query_messages(Req) ->
  {ok, Messages} = gb_server:list_messages(),
  {ok, Guests} = gb_server:list_guests(),

  MessageList = lists:map(
    fun ({message, MessageId, GuestId, GuestName, Text, CreatedAt}) ->
      #{id       => MessageId,
        guest_id => GuestId,
        guest_name => GuestName,
        text     => Text,
        created_at => iso_8601_fmt(CreatedAt)}
    end,
    Messages
  ),
  GuestList = lists:map(
    fun ({guest, UserId, Username, _}) ->
      #{id       => UserId,
        username => Username}
    end,
    Guests
  ),

  {ok, jsx:encode(#{messages => MessageList,
                    guests => GuestList})}.

get_message(Id, Req) ->
  {ok, <<"ok">>}.

create_message(Req) ->
  {ok, Data, _} = cowboy_req:body(Req),
  GuestInfo = jsx:decode(Data, [return_maps]),
  #{
    <<"name">> := Name,
    <<"contact">> := Contact,
    <<"message">> := Text
  } = GuestInfo,

  {ok, {guest, GuestId, Name, Contact}} = gb_server:add_guest({guest, Name, Contact}),
  {ok, {message, MessageId, GuestId, Text, CreatedAt}} = gb_server:create_message({message, GuestId, Text}),

%%  Broadcast to sse clients
  ClientProcessList = ets:tab2list(sse_clients),
  lists:foreach(
    fun ({ClientPid, _}) ->
      Pid = list_to_pid(ClientPid),
      Pid! {update, calendar:universal_time()}
    end,
    ClientProcessList),

  {ok, jsx:encode(#{
    guest => #{
      id => GuestId,
      name => Name,
      contact => Contact
    },
    message => #{
      id         => MessageId,
      guest_id   => GuestId,
      text       => Text,
      created_at => list_to_binary(iso_8601_fmt(CreatedAt))
    }
  })}.

iso_8601_fmt(DateTime) ->
  {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
  io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
    [Year, Month, Day, Hour, Min, Sec]).