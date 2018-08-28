-module(erlang_testing).

% Erlang Distribution
-export([
    start_distrib/1,
    start_distrib/2,
    start_distrib/3,
    stop_distrib/0
]).

% Test slave nodes
-export([
    slaves_setup/1,
    ct_slaves_setup/1,
    cleanup_slaves/1,
    ct_cleanup_slaves/1
]).

% Application
-export([
    stop_extra_applications/0
]).

% @doc This will start the distribution ( shortnames , longnames ) with the supplied Nodename.
% @end

-spec start_distrib(node()) -> {ok, pid()}.
start_distrib(Name) ->
    do_start_distrib([Name]).

-spec start_distrib(node(), shortnames | longnames) -> {ok, pid()}.
start_distrib(Name, NameType) ->
    do_start_distrib([Name, NameType]).

-spec start_distrib(node(), shortnames | longnames, pos_integer()) -> {ok, pid()}.
start_distrib(Name, NameType, Ticktime) ->
    do_start_distrib([Name, NameType, Ticktime]).

-spec do_start_distrib(list()) -> {ok, pid()}.
do_start_distrib(A) ->
    case distrib_already_started(node()) of
        true ->
            {ok, whereis(net_sup)};
        false ->
            % [] = os:cmd("epmd -daemon"),
            net_kernel:start(A)
    end.

-spec distrib_already_started(node()) -> boolean().
distrib_already_started('nonode@nohost') ->
    false;
distrib_already_started(_) ->
    true.

-spec stop_distrib() -> ok | {error, not_allowed | not_found}.
stop_distrib() ->
    net_kernel:stop().

-spec slaves_setup(list(tuple())) -> list(node()).
slaves_setup(Slaves) when is_list(Slaves) ->
    lists:map(
        fun({H})       -> slave_node_start(H);
           ({H, N})    -> slave_node_start(H, N);
           ({H, N, A}) -> slave_node_start(H, N, A)
        end, Slaves).

%% Slave
-spec slave_node_start(inet:hostname()) -> node().
slave_node_start(Host) ->
    {ok, SlaveName} = slave:start(Host),
    SlaveName.

-spec slave_node_start(inet:hostname(), atom() | string()) -> node().
slave_node_start(Host, Name) ->
    {ok, SlaveName} = slave:start(Host, Name),
    SlaveName.

-spec slave_node_start(inet:hostname(), atom() | string(), string()) -> node().
slave_node_start(Host, Name, Args) ->
    {ok, SlaveName} = slave:start(Host, Name, Args),
    SlaveName.

-spec cleanup_slaves(list(node())) -> boolean().
cleanup_slaves(Slaves) ->
    lists:all(fun(SlaveNodeName) ->
        ok == slave:stop(SlaveNodeName)
        % case slave:stop(SlaveNodeName) of
        %     ok ->
        %         true;
        %     Reason ->
        %         error_logger:error_msg(
        %             "Could not stop slave ~p Reason ~p",
        %             [SlaveNodeName, Reason]
        %         ),
        %         false
        % end
    end, Slaves).

%% CT slave
-spec ct_slaves_setup(list(tuple())) -> list(node()).
ct_slaves_setup(Slaves) when is_list(Slaves) ->
    lists:map(
        fun({N}) ->
                ct_slave_node_start(N);
           ({HOrN, NOrOpts}) ->
                ct_slave_node_start(HOrN, NOrOpts);
           ({H, N, O}) ->
                ct_slave_node_start(H, N, O)
        end, Slaves).

-spec ct_slave_node_start(atom()) -> node().
ct_slave_node_start(Node) when is_atom(Node) ->
    {ok, SlaveName} = ct_slave:start(Node),
    SlaveName.

-spec ct_slave_node_start(atom(), atom() | list()) -> node().
ct_slave_node_start(HostOrNode, NodeOrOpts) when is_atom(HostOrNode) ->
    {ok, SlaveName} = ct_slave:start(HostOrNode, NodeOrOpts),
    SlaveName.

-spec ct_slave_node_start(atom(), atom() | list(), list()) -> node().
ct_slave_node_start(Host, Node, Opts) when is_atom(Host), is_atom(Node) ->
    {ok, SlaveName} = ct_slave:start(Host, Node, Opts),
    SlaveName.

-spec ct_cleanup_slaves(list(node())) -> boolean().
ct_cleanup_slaves(Slaves) ->
    lists:all(fun(SlaveNodeName) ->
        CtSlaveStopResponse = ct_slave:stop(SlaveNodeName),
        io:format("CtSlaveStopResponse ~p\n", [CtSlaveStopResponse]),
        {ok, SlaveNodeName} == CtSlaveStopResponse
        % case slave:stop(SlaveNodeName) of
        %     ok ->
        %         true;
        %     Reason ->
        %         error_logger:error_msg(
        %             "Could not stop slave ~p Reason ~p",
        %             [SlaveNodeName, Reason]
        %         ),
        %         false
        % end
    end, Slaves).

-spec stop_extra_applications() -> list(ok).
stop_extra_applications() ->
    [ ok = application:stop(App) ||
        {App,_ErtsVsn,_Vsn}
        <- application:which_applications(), App /= kernel andalso App /= stdlib
    ].