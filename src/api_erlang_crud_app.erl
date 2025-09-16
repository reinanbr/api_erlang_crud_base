-module(api_erlang_crud_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    % Get configuration
    {ok, Port} = application:get_env(api_erlang_crud, port),
    {ok, DbFile} = application:get_env(api_erlang_crud, db_file),
    
    % Ensure priv directory exists
    ok = filelib:ensure_dir(DbFile),
    
    % Define routes
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/register", register_handler, []},
            {"/login", login_handler, []},
            {"/balance", balance_handler, []},
            {"/buy", transaction_handler, []},
            {"/sell", transaction_handler, []}
        ]}
    ]),
    
    % Start Cowboy HTTP server
    case cowboy:start_clear(http, 
        [{port, Port}], 
        #{env => #{dispatch => Dispatch}}
    ) of
        {ok, _} -> ok;
        {error, eaddrinuse} -> {error, eaddrinuse};
        Error -> Error
    end,
    
    % Start supervisor
    api_erlang_crud_sup:start_link(DbFile).

stop(_State) ->
    ok = cowboy:stop_listener(http),
    ok.
