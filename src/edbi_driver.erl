%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Driver utilities
%% @end
%%%-------------------------------------------------------------------
-module(edbi_driver).

%% API
-export([connect_mfa/3
         ,terminate/2]).
%%====================================================================
%% API
%%====================================================================

connect_mfa(dummy, Pool, Options) when is_list(Options) ->
    {edbi_dummy, connect_link, [Pool, Options]};
connect_mfa(Driver, _, _) ->
    erlang:error({unknown_driver, Driver}).

terminate(dummy, Pid) ->
    Pid ! shutdown;
terminate(Driver, _) ->
    erlang:error({unknown_driver, Driver}).

%%====================================================================
%% Internal functions
%%====================================================================
