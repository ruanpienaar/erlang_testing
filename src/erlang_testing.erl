-module(erlang_testing).
-export([
    start_distrib/1,
    start_distrib/2,
    start_distrib/3,
    stop_distrib/0
]).


% @doc This will start the distribution ( shortnames , longnames ) with the supplied Nodename.
% @end

-spec start_distrib(node()) -> ok.
start_distrib(Name) ->
    do_start_distrib([Name]).

-spec start_distrib(node(), shortnames | longnames) -> ok.
start_distrib(Name, NameType) ->
    do_start_distrib([Name, NameType]).

-spec start_distrib(node(), shortnames | longnames, pos_integer()) -> ok.
start_distrib(Name, NameType, Ticktime) ->
    do_start_distrib([Name, NameType, Ticktime]).

do_start_distrib(A) ->
    case distrib_already_started(node()) of
        true ->
            ok;
        false ->
            [] = os:cmd("epmd -daemon"),
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