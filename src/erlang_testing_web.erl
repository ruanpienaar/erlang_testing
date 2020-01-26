-module(erlang_testing_web).

-export([
    url_req/2,
    long_url_req/2,
    close_long_req/1
]).

-spec url_req(holster:req_type(), http_uri:uri()) -> {ok, {pos_integer(), list(), binary()}}.
url_req(ReqType, Url) ->
    case
        holster:simple_proc_req(
            ReqType,
            Url,
            #{
                retry => 0,
                retry_timeout => 0,
                domain_lookup_timeout => 250,
                connect_timeout => 250,
                http_opts => #{
                    closing_timeout => 250
                }
            }
        )
    of
        {response, {StatusCode, ResponseHeaders, ResponseData}} ->
            {ok, {StatusCode, ResponseHeaders, ResponseData}};
        {response, Response} ->
            {error, Response};
        X ->
            X
    end.

-spec long_url_req(holster:req_type(), http_uri:uri()) -> {ok, pid(), {pos_integer(), list(), binary()}}.
long_url_req(ReqType, Url) ->
    case
        holster:stay_connected_req(
            ReqType,
            Url,
            #{
                retry => 10000,
                retry_timeout => 0,
                domain_lookup_timeout => 250,
                connect_timeout => 250,
                http_opts => #{
                    closing_timeout => 250
                }
            }
        )
    of
    %% Here we might want to monitor this pid...
        {{ok, Pid}, {response, {StatusCode, ResponseHeaders, ResponseData}}} ->
            {ok, Pid, {StatusCode, ResponseHeaders, ResponseData}};
        {{ok, Pid}, {response, Response}} ->
            {ok, Pid, Response}
    end.

close_long_req(Pid) ->
    holster:close_req(Pid).
