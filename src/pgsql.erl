%%% File    : pgsql.erl
%%% Author  : Christian Sunesson <chsu79@gmail.com>
%%% Description : PostgresQL interface
%%% Created : 11 May 2005

%%
%% API for accessing the postgres driver.
%%

-module(pgsql).
-export([connect/1, connect/4, connect/5,
         connect_link/1]).

-export([squery/2, squery/3,
	 pquery/3, 
	 terminate/1, 
	 prepare/3, unprepare/2, 
	 execute/3, execute/4, transaction/2, transaction/3]).


connect(Host, Database, User, Password) ->
    connect([{database, Database},
	     {host, Host},
	     {user, User},
	     {password, Password}]).

connect(Host, Database, User, Password, Port) ->
    connect([{database, Database},
	     {host, Host},
	     {user, User},
	     {port, Port},
	     {password, Password}]).

connect(Options) ->
    pgsql_proto:start(Options).

connect_link(Options) ->
    pgsql_proto:start_link(Options).

%% Close a connection
terminate(Db) ->
    gen_server:call(Db, terminate).

%%% In the "simple query" protocol, the frontend just sends a 
%%% textual query string, which is parsed and immediately 
%%% executed by the backend.  

%% A simple query can contain multiple statements (separated with a semi-colon),
%% and each statement's response.

%%% squery(Db, Query) -> {ok, Results} | ... no real error handling
%%% Query = string()
%%% Results = [Result]
%%% Result = {"SELECT", RowDesc, ResultSet} | ...
squery(Db, Query) ->
    squery(Db, Query, infinity).

%%% squery(Db, Query) -> {ok, Results} | ... no real error handling
%%% Query = string()
%%% Timeout = integer() | infinity
%%% Results = [Result]
%%% Result = {"SELECT", RowDesc, ResultSet} | ...
squery(Db, Query, Timeout) ->
    gen_server:call(Db, {squery, Query}, Timeout).


%%% In the "extended query" protocol, processing of queries is 
%%% separated into multiple steps: parsing, binding of parameter
%%% values, and execution. This offers flexibility and performance
%%% benefits, at the cost of extra complexity.

%%% pquery(Db, Query, Params) -> {ok, Command, Status, NameTypes, Rows} | timeout | ...
%%% Query = string()
%%% Params = [term()]
%%% Command = string()
%%% Status = idle | transaction | failed_transaction
%%% NameTypes = [{ColName, ColType}]
%%% Rows = [list()]
pquery(Db, Query, Params) ->
    gen_server:call(Db, {equery, {Query, Params}}).

%%% prepare(Db, Name, Query) -> {ok, Status, ParamTypes, ResultTypes}
%%% Status = idle | transaction | failed_transaction
%%% ParamTypes = [atom()]
%%% ResultTypes = [{ColName, ColType}]
prepare(Db, Name, Query) when is_atom(Name) ->
    gen_server:call(Db, {prepare, {atom_to_list(Name), Query}}).

%%% unprepare(Db, Name) -> ok | timeout | ...
%%% Name = atom()
unprepare(Db, Name) when is_atom(Name) ->
    gen_server:call(Db, {unprepare, atom_to_list(Name)}).

%%% execute(Db, Name, Params) -> {ok, Result} | timeout | ...
%%% Result = {'INSERT', NRows} |
%%%          {'DELETE', NRows} |
%%%          {'SELECT', ResultSet} |
%%%          ...
%%% ResultSet = [Row]
%%% Row = list()
execute(Db, Name, Params) when is_atom(Name), is_list(Params) ->
    gen_server:call(Db,{execute, {atom_to_list(Name), Params}}).

%%% execute(Db, Name, Params, Timeout) -> {ok, Result} | timeout | ...
%%% Result = {'INSERT', NRows} |
%%%          {'DELETE', NRows} |
%%%          {'SELECT', ResultSet} |
%%%          ...
%%% ResultSet = [Row]
%%% Row = list()
%%% Timeout = integer() | infinity
execute(Db, Name, Params, Timeout) when is_atom(Name), is_list(Params) ->
    gen_server:call(Db,{execute, {atom_to_list(Name), Params}}, Timeout).


%% @doc Executes all statements that are execute in Fun in an transaction
%% with inifnity timeout.
%% @see transaction/3
transaction(Db, Fun) ->  
  transaction(Db, Fun, infinity).

%% @doc Executes all statements that are execute in Fun in an transaction..
%% If an error occurs/is thrown or if the fun returns error, or {error, Reason},
%% the transaction is rolled back. 
%% Returns {error, {rollback, RollbackResult, Error}}, if an error occurred and
%% the transaction was rolled back. RollbackResult = term() is the result of the
%% rollback, e.g. {ok, "ROLLBACK"} if the rollback was successfull, Error = term()
%% contains the reason for rollback.
%% Returns {error, {begin_transaction, Reason}} if the transaction could not be started.
%% Returns the return value from the Fun, if the transaction was commited successfully.
%% 
%% e.g:
%% pgsql:prepare(Db, stm1, "INSERT INTO foo (bar) VALUES ($1)"),
%% pgsql:prepare(Db, stm2, "INSERT INTO bar (foo) VALUES ($1)"),
%% Fun = fun() -> 
%%  case pgsql:execute(Db, stm1, [1]) of
%%    {ok,{'INSERT',1}} ->
%%      case pgsql:execute(Db, stm2, [2]) of
%%        {ok,{'INSERT',1}} -> ok; % commit transaction and return ok
%%        Unexpected -> error % rollback the transaction
%%      end;
%%    Unexpected -> {error, Unexpected} % rollback the transaction
%%  end
%% end,
%% case pgsql:transaction(Db, Fun) of
%%  ok -> io:format("transaction commited");
%%  {error, {begin_transaction, _}} -> io:format("could not start transaction.");
%%  {error, {rollback, RollbackResult, Error}} ->
%%    case RollbackResult of 
%%      {ok, "ROLLBACK"} -> io:format("transaction was successfully rolled back.");
%%      RollbackError -> io:format("transaction not commited, but rollback did not work.")
%%    end
%% end
%%
%% NOTICE: this transaction function does not block the Db connection thread, when starting the transaction
%% and executing the statements in that transaction. so make sure, that you DO NOT EXECUTE ANY STATEMENTS
%% AT THE SAME DB CONNECTION FROM ANOTHER THREAD than the tread that invokes this transaction function, 
%% because otherwise they can be executed in the current transaction.
%%
%%
%% @spec transaction(Db::pid(), Fun::function(), Timeout::integer() | infinity) ->
%%   {error, {rollback, RollbackResult, Error}} | {error, {begin_transaction, Unexpected}} | FunResult
transaction(Db, Fun, Timeout) ->
  case squery(Db, "BEGIN", Timeout) of
    {ok,["BEGIN"]} ->
      try
        case Fun() of
          error ->
            rollback(Db, error, Timeout);
          {error, Reason} ->
            rollback(Db, Reason, Timeout);
          Result ->
            case squery(Db, "COMMIT", Timeout) of
              {ok,["COMMIT"]} -> 
                Result;
              Unexpected -> 
                rollback(Db, Unexpected, Timeout)
            end
        end
      catch Class:Error ->
        rollback(Db, {Class, Error}, Timeout)
      end;
    Unexpected -> 
      {error, {begin_transaction, Unexpected}}
  end.

rollback(Db, Error, Timeout) ->
  RollbackResult = squery(Db, "ROLLBACK", Timeout),
  {error, {rollback, RollbackResult, Error}}.
