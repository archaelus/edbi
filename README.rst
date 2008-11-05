==================================================
EDBI - A database independent interface for Erlang
==================================================

EDBI is intended as a database independence and connection pooling
library for erlang programs.

Drivers
=======

dummy
  ``edbi_dummy`` A dummy driver for EDBI testing that just writes SASL logs on
  startup
pgsql
  ``pgsql`` The `Process One <http://process-one.net>`_ Postgresql
  driver modified for use with EDBI.
mysql
  ``mysql`` The `Process One <http://process-one.net>`_ MySQL driver
  modifed for use with EDBI. [1]_

.. [1] Note that this driver isn't tested yet.


Configuration
=============

EDBI uses OTP application configuration, so you can just put::

  {edbi, [{pools, pools()}]}.

in your ``system.config`` file.

The syntax of the pools env value is described in edoc type format as
follows::

  pools() = [pool(),...].
  pool()  = {PoolName::term(),
             Driver::atom(),
             Options::term(),
             Count::integer()}.

PoolName
  the term used to identify and refer to the pool.
Driver
  the name of the backend to use in the pool.
Options 
  a proplist of options to pass to the backend.
Count 
  the number of connections to maintain to the database.

Examples
--------
A pool called dummy_pool of 10 dummy drivers.::

  {dummy_pool, dummy, [], 10}


A pool (``mydb``) of 10 connections to the local postgres database
``mydb`` as ``myuser``.::

  {mydb, pgsql, [{database, "mydb"},
                 {host, "localhost"},
                 {user, "myuser"},
                 {password, ""}],
         10}

===
Use
===

Currently the API is extremely basic and only allows you to retreive
connection PIDs to use with the driver backend.::

  1> ConnectionPid = edbi:connection(PoolName),
  1> pgsql:squery(ConnectionPid, "select 1").  
  {ok,[{"SELECT",[{"?column?",text,0,23,4,-1,0}],[["1"]]}]}

