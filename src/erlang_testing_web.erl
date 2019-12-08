-module(erlang_testing_web).

-export([
    url_req/1,
    long_url_req/1,
    close_long_req/1
]).

url_req(Url) ->
    case
        holster:once_off_req(
            Url,
            #{
                retry => 0,
                retry_timeout => 0,
                domain_lookup_timeout => 250,
                connect_timeout => 250,
                http_opts => #{
                    closing_timeout => 250
                }
            })
    of
        {response, {StatusCode, ResponseHeaders, ResponseData}} ->
            {ok, {StatusCode, ResponseHeaders, ResponseData}};
        {response, Response} ->
            {error, Response};
        X ->
            X
    end.

long_url_req(Url) ->
    case
        holster:long_running_req(
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
