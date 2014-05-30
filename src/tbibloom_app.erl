%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the tbibloom application.

-module(tbibloom_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for tbibloom.
start(_Type, _StartArgs) ->
    tbibloom_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for tbibloom.
stop(_State) ->
    ok.
