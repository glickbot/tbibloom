-module(tbibloom_riakc_pool_worker).

-export([start_link/1, stop/1]).

start_link(Args) ->
    Host = proplists:get_value(riak_ip, Args, "127.0.0.1"),
	Port = proplists:get_value(riak_pb_port, Args, 8087),
	StartOptions = [{connect_timeout, 10000},
                    {auto_reconnect, true}],
    riakc_pb_socket:start_link(Host, Port, StartOptions).

stop(undefined) ->
	ok;
stop(Worker) ->
	riakc_pb_socket:stop(Worker).