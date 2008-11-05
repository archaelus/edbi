%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Dummy EDBI Driver
%% @end
-module(edbi_dummy).

-export([connect/1
         ,init/1
         ,wake/0]).

connect(Options) ->
    proc_lib:start_link(?MODULE, init, [[self(), Options]]).

init([Parent,Options]) ->
    error_logger:info_msg("EDBI Dummy connection ~p: Starting up~nOptions:~p~n",
                          [self(), Options]),
    proc_lib:init_ack(Parent, {ok, self()}),
    erlang:hibernate(?MODULE, wake, []).

wake() ->
    error_logger:info_msg("EDBI Dummy connection ~p: Shutting down", [self()]).
    

