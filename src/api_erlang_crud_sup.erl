-module(api_erlang_crud_sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(DbFile) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [DbFile]).

init([DbFile]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    
    ChildSpecs = [
        #{
            id => db_server,
            start => {db_server, start_link, [DbFile]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [db_server]
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}.
