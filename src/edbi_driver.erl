%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Driver utilities
%% @end
%%%-------------------------------------------------------------------
-module(edbi_driver).

%% API
-export([connect/2
         ,connect_mfa/2
         ,terminate/2]).

%%====================================================================
%% API
%%====================================================================

connect(pgsql, Options) ->
    pgsql:connect(Options);
connect(dummy, Options) ->
    edbi_dummy:connect(Options);
connect(Driver, _) ->
    erlang:error({unknown_driver, Driver}).

connect_mfa(pgsql, Options) ->
    {pgsql, connect_link, [Options]};
connect_mfa(dummy, Options) ->
    {edbi_dummy, connect, Options};
connect_mfa(Driver, _) ->
    erlang:error({unknown_driver, Driver}).

terminate(pgsql, Pid) ->
    pgsql:terminate(Pid);
terminate(dummy, Pid) ->
    Pid ! shutdown;
terminate(Driver, _) ->
    erlang:error({unknown_driver, Driver}).

%%====================================================================
%% Internal functions
%%====================================================================
