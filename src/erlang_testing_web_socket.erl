-module(erlang_testing_web_socket).
-include_lib("eunit/include/eunit.hrl").
-export([
    start_client/3,
    send_ws_request/3,
    subscribe_to_ws_msgs/2,
    unsubscribe_to_ws_msgs/2,
    flush/0,
    cleanup_client_pid/1
]).

-define(CLIENT_PING_INTERVAL, 30 * 1000).

%% @doc
%% This is a short description on how to use this module.
%%
%% This utility module assist tests with connecting to and sending
%% plus receiving websockets request/response messages.
%% Allowing your test module to stay simple, and showcase the requests
%% with the subsequent expected responses.
%%
%% Steps below for using this in Eunit/Common Test.
%%
%% Add below to your setup fixture OR to your test:
%%  _ClientPid = erlang_testing_web_socket:start_client("localhost", 8080).
%%
%% Add below to your cleanup fixture:
%%  erlang_testing_web_socket:cleanup_client_pid(ClientPid)
%%
%% Foreachx example:
%%
%% some_unit_test_() ->
%%     {foreachx,
%%      fun(_Test) ->
%%         {ok, ClientPid, StartedApps} = erlang_testing_web_socket:start_client("localhost", 8080, "ws_url")
%%      end,
%%      fun(_Test, {ok, ClientPid, StartedApps}) ->
%%         erlang_testing_web_socket:cleanup_client_pid(ClientPid),
%%         lists:foreach(fun(App) -> ok = application:stop(App) end, lists:reverse(StartedApps))
%%      end,
%%      [
%%         {"test some_unit_test",
%%             fun(_Test, ClientPid) ->
%%                 %% Refer to some_unit_test/1 below
%%                 ?_test(some_unit_test(ClientPid))
%%             end
%%         }
%%      ]
%%     }.
%%
%% Simple test example:
%%
%% some_unit_test() ->
%%     ClientPid = erlang_testing_web_socket:start_client("localhost", 8080, "ws_url"),
%%     erlang_testing_web_socket:send_ws_request(self(), ClientPid, <<"ping">>),
%%     receive
%%         X ->
%%             ?assertEqual(
%%                 {response,{text,<<"pong">>}},
%%                 X
%%             ),
%%             erlang_testing_web_socket:cleanup_client_pid(ClientPid)
%%     after
%%         1000 ->
%%             erlang_testing_web_socket:cleanup_client_pid(ClientPid),
%%             erlang:exit(self(), {test, ?FUNCTION_NAME, failed, line, ?LINE})
%%     end.
%% @end

%%------------------------------------------------------------------------------
%% API

%% @doc
%% connect to the hostname and port and upgrade the connection to websocket
%% @end
start_client(Hostname, Port, WsPath) ->
    % False if you've not started gun! So start gun before this...
    {gun, _} =
        lists:keyfind(gun, 1, element(2, lists:keyfind(started, 1, application:info()))),
    Pid = spawn_link(fun() -> worker_init(Hostname, Port, WsPath) end),
    Pid ! {wait_for_init, self()},
    receive
        ok ->
            ok
    after
        1000 ->
            ?debugFmt("test code failed MOD ~p LINE ~p\n", [?MODULE, ?LINE])
    end,
    {ok, Pid}.

%% @doc
%% send a web socket request to the connected websocket client pid.
%% @end
send_ws_request(RequesterPid, ClientPid, ReqJson) ->
    ClientPid ! {send_ws_request, RequesterPid, ReqJson}.

%% @doc
%% subscribe to subsequent websocket messages received on client side.
%% @end
subscribe_to_ws_msgs(RequesterPid, ClientPid) ->
    ClientPid ! {subscribe_to_ws_msgs, RequesterPid}.

%% @doc
%% Unsubscribe from websocket client, to stop receiving responses.
%% @end
unsubscribe_to_ws_msgs(RequesterPid, ClientPid) ->
    ClientPid ! {unsubscribe_to_ws_msgs, RequesterPid}.

%% @doc
%% Let gathered messages flush out
%% @end
flush() ->
    receive
        _X ->
            flush()
    after
        100 ->
            ok
    end.

cleanup_client_pid(ClientPid) ->
    true = erlang:unlink(ClientPid),
    ClientPid ! {get_conn_pid, self()},
    receive
        {ok, ConnPid} ->
            ok = gun:close(ConnPid)
    after
        1000 ->
            erlang:exit(self(), {failed, ?FUNCTION_NAME, line, ?LINE})
    end,
    ?assertEqual(undefined, erlang:process_info(ClientPid)).

%%------------------------------------------------------------------------------
%% Internal

%% NB!
%% Only handle the specific msg receives, so that the other messages
%% are queued up in the mailbox, until init is over.
%% Except for get_conn_pid, where a test might want to close early
worker_init(Hostname, Port, WsPath) ->
    ?debugFmt("[~p][~p]", [?MODULE, ?FUNCTION_NAME]),
    {ok, ConnPid} = gun:open(Hostname, Port),
    true = erlang:link(ConnPid),
    receive
        {gun_up, ServerPid, Proto} ->
            ?debugFmt("Gun connection ~p up [~p]", [ServerPid, Proto]),
            ok
    after
        1000 ->
            erlang:exit(self(), {{failed, ?FUNCTION_NAME, line, ?LINE}, could_not_open_connection})
    end,
    Ref = gun:ws_upgrade(ConnPid, WsPath),
    receive
        {gun_upgrade, ServerPid2, Ref, Proto2, Headers} ->
            ?debugFmt("Gun connection ~p upgraded [~p]\n[~p]", [ServerPid2, Proto2, Headers]),
            ok
    after
        1000 ->
            erlang:exit(self(), {{failed, ?FUNCTION_NAME, line, ?LINE}, could_not_upgrade_connection})
    end,
    % start periodic pinging
    {ok, TRef} =
        timer:apply_interval(
            ?CLIENT_PING_INTERVAL, gun, ws_send, [ConnPid, {text, <<"ping">>}]),
    receive
        {wait_for_init, RequesterPid} ->
            RequesterPid ! ok
    after
        1000 ->
            erlang:exit(self(), {failed, ?FUNCTION_NAME, line, ?LINE})
    end,
    worker(#{tref => TRef,
             ref => Ref,
             conn_pid => ConnPid}).

worker(#{conn_pid := ConnPid} = State) ->
    ?debugFmt("[~p][~p] worker loop", [?MODULE, ?FUNCTION_NAME]),
    receive
        {get_conn_pid, RequesterPid} ->
            ?debugFmt("[~p][~p] ~p\n",[?MODULE, ?FUNCTION_NAME, RequesterPid]),
            RequesterPid ! {ok, ConnPid},
            worker(State);
        {send_ws_request, RequesterPid, SpecialReq} when SpecialReq == close orelse
                                                         SpecialReq == ping orelse
                                                         SpecialReq == pong ->
            ?debugFmt("sending unit test request ~p \n", [SpecialReq]),
            ok = gun:ws_send(ConnPid, SpecialReq),
            worker(State#{requester_pid => RequesterPid});
        {send_ws_request, RequesterPid, ReqJson} when is_binary(ReqJson) ->
            ?debugFmt("sending unit test request ~p \n", [ReqJson]),
            ok = gun:ws_send(ConnPid, {text, ReqJson}),
            worker(State#{requester_pid => RequesterPid});
        {subscribe_to_ws_msgs, RequesterPid} ->
            ?debugFmt("[~p][~p] ~p\n",[?MODULE, ?FUNCTION_NAME, RequesterPid]),
            worker(State#{ subs_pid => RequesterPid });
        {unsubscribe_to_ws_msgs, RequesterPid} ->
            ?debugFmt("[~p][~p] ~p\n",[?MODULE, ?FUNCTION_NAME, RequesterPid]),
            worker(maps:without([subs_pid], State));
        % Should we match _Ref ?
        {gun_ws, ConnPid, _Ref, Response} ->
            ?debugFmt("[~p][~p] ConnPid ~p\n",[?MODULE, ?FUNCTION_NAME, ConnPid]),
            worker(should_publish_or_forward(State, Response));
        X ->
            ?debugFmt("Unknown receive ~p \n", [X]),
            worker(State)
    end.

should_publish_or_forward(State, Response) ->
    case maps:is_key(subs_pid, State) of
        true ->
            #{ requester_pid := SubsPid } = State,
            SubsPid ! {response, Response},
            State;
        false ->
            case maps:is_key(requester_pid, State) of
                true ->
                    ?debugFmt("WS response forwarding to ClientPid ~p\n", [Response]),
                    #{ requester_pid := RequesterPid } = State,
                    RequesterPid ! {response, Response},
                    maps:without([requester_pid], State);
                false ->
                    ?debugFmt("WS response ~p\n", [Response]),
                    State
            end
    end.
