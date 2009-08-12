%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Pool definitions
%% @end

-ifndef(edbi_pool).
-define(edbi_pool, true).

-record(edbi_pool, {name, driver, pool_size}).

-endif. %logging
