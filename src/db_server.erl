-module(db_server).
-behaviour(gen_server).

%% API
-export([start_link/1, create_user/2, authenticate_user/2, get_user_balance/1, 
         update_balance/2, create_transaction/4, get_user_by_username/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {users_table, transactions_table}).

%% API Functions
start_link(_DbFile) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_user(Username, PasswordHash) ->
    gen_server:call(?MODULE, {create_user, Username, PasswordHash}).

authenticate_user(Username, Password) ->
    gen_server:call(?MODULE, {authenticate_user, Username, Password}).

get_user_balance(UserId) ->
    gen_server:call(?MODULE, {get_user_balance, UserId}).

update_balance(UserId, NewBalance) ->
    gen_server:call(?MODULE, {update_balance, UserId, NewBalance}).

create_transaction(UserId, Type, Amount, Timestamp) ->
    gen_server:call(?MODULE, {create_transaction, UserId, Type, Amount, Timestamp}).

get_user_by_username(Username) ->
    gen_server:call(?MODULE, {get_user_by_username, Username}).

%% gen_server callbacks
init([]) ->
    UsersTable = ets:new(users, [set, protected, named_table]),
    TransactionsTable = ets:new(transactions, [set, protected, named_table]),
    {ok, #state{users_table = UsersTable, transactions_table = TransactionsTable}}.

handle_call({create_user, Username, PasswordHash}, _From, State) ->
    UserId = generate_uuid(),
    UserKey = {user, Username},
    case ets:lookup(users, UserKey) of
        [] ->
            User = {UserId, Username, PasswordHash, 1000.0, erlang:system_time(second)},
            ets:insert(users, {UserKey, User}),
            ets:insert(users, {{user_id, UserId}, User}),
            {reply, {ok, UserId}, State};
        _ ->
            {reply, {error, username_exists}, State}
    end;

handle_call({authenticate_user, Username, Password}, _From, State) ->
    UserKey = {user, Username},
    case ets:lookup(users, UserKey) of
        [{_, {UserId, _, StoredHash, _, _}}] ->
            case verify_password(Password, StoredHash) of
                true -> {reply, {ok, UserId}, State};
                false -> {reply, {error, invalid_credentials}, State}
            end;
        [] ->
            {reply, {error, user_not_found}, State}
    end;

handle_call({get_user_balance, UserId}, _From, State) ->
    UserIdKey = {user_id, UserId},
    case ets:lookup(users, UserIdKey) of
        [{_, {_, _, _, Balance, _}}] ->
            {reply, {ok, Balance}, State};
        [] ->
            {reply, {error, user_not_found}, State}
    end;

handle_call({update_balance, UserId, NewBalance}, _From, State) ->
    UserIdKey = {user_id, UserId},
    case ets:lookup(users, UserIdKey) of
        [{_, {UserId, Username, Hash, _, Created}}] ->
            NewUser = {UserId, Username, Hash, NewBalance, Created},
            ets:insert(users, {UserIdKey, NewUser}),
            ets:insert(users, {{user, Username}, NewUser}),
            {reply, ok, State};
        [] ->
            {reply, {error, user_not_found}, State}
    end;

handle_call({create_transaction, UserId, Type, Amount, Timestamp}, _From, State) ->
    TransactionId = generate_uuid(),
    Transaction = {TransactionId, UserId, Type, Amount, Timestamp, erlang:system_time(second)},
    ets:insert(transactions, {{transaction, TransactionId}, Transaction}),
    {reply, {ok, TransactionId}, State};

handle_call({get_user_by_username, Username}, _From, State) ->
    UserKey = {user, Username},
    case ets:lookup(users, UserKey) of
        [{_, {UserId, Username, _, Balance, _}}] ->
            {reply, {ok, #{id => UserId, username => Username, balance => Balance}}, State};
        [] ->
            {reply, {error, user_not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
generate_uuid() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b", 
                                [A, B, C, D, E])).

verify_password(Password, Hash) ->
    % Simplified password verification - in production use proper hashing
    Hash =:= hash_password(Password).

hash_password(Password) ->
    % Simplified hashing - in production use bcrypt or similar
    crypto:hash(sha256, Password).
