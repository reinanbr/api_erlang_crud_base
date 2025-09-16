-module(transaction_handler).
-export([init/2]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    handle_request(Method, Path, Req0, Opts).

handle_request(<<"POST">>, <<"/buy">>, Req0, Opts) ->
    handle_transaction(buy, Req0, Opts);

handle_request(<<"POST">>, <<"/sell">>, Req0, Opts) ->
    handle_transaction(sell, Req0, Opts);

handle_request(_, _, Req0, Opts) ->
    Response = jiffy:encode(#{
        <<"success">> => false,
        <<"error">> => <<"Method not allowed">>
    }),
    Req1 = cowboy_req:reply(405, 
        #{<<"content-type">> => <<"application/json">>}, 
        Response, Req0),
    {ok, Req1, Opts}.

handle_transaction(Type, Req0, Opts) ->
    case get_user_from_token(Req0) of
        {ok, UserId} ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            try
                Json = jiffy:decode(Body, [return_maps]),
                Amount = maps:get(<<"amount">>, Json),
                
                case validate_amount(Amount) of
                    ok ->
                        process_transaction(Type, UserId, Amount, Req1, Opts);
                    {error, Error} ->
                        ResponseError = jiffy:encode(#{
                            <<"success">> => false,
                            <<"error">> => Error
                        }),
                        ReqError = cowboy_req:reply(400, 
                            #{<<"content-type">> => <<"application/json">>}, 
                            ResponseError, Req1),
                        {ok, ReqError, Opts}
                end
            catch
                _:_ ->
                    ResponseException = jiffy:encode(#{
                        <<"success">> => false,
                        <<"error">> => <<"Invalid JSON">>
                    }),
                    ReqException = cowboy_req:reply(400, 
                        #{<<"content-type">> => <<"application/json">>}, 
                        ResponseException, Req1),
                    {ok, ReqException, Opts}
            end;
        {error, Error} ->
            Response = jiffy:encode(#{
                <<"success">> => false,
                <<"error">> => Error
            }),
            Req1 = cowboy_req:reply(401, 
                #{<<"content-type">> => <<"application/json">>}, 
                Response, Req0),
            {ok, Req1, Opts}
    end.

process_transaction(buy, UserId, Amount, Req, Opts) ->
    case db_server:get_user_balance(UserId) of
        {ok, CurrentBalance} when CurrentBalance >= Amount ->
            NewBalance = CurrentBalance - Amount,
            Timestamp = iso8601_timestamp(),
            
            % Start transaction - update balance and create transaction record
            case db_server:update_balance(UserId, NewBalance) of
                ok ->
                    case db_server:create_transaction(UserId, buy, Amount, Timestamp) of
                        {ok, TransactionId} ->
                            Response = jiffy:encode(#{
                                <<"success">> => true,
                                <<"message">> => <<"Purchase successful">>,
                                <<"transaction_id">> => list_to_binary(TransactionId),
                                <<"new_balance">> => NewBalance,
                                <<"amount">> => Amount,
                                <<"type">> => <<"buy">>
                            }),
                            Req1 = cowboy_req:reply(200, 
                                #{<<"content-type">> => <<"application/json">>}, 
                                Response, Req),
                            {ok, Req1, Opts};
                        {error, _} ->
                            % Rollback balance update
                            db_server:update_balance(UserId, CurrentBalance),
                            Response = jiffy:encode(#{
                                <<"success">> => false,
                                <<"error">> => <<"Transaction failed">>
                            }),
                            Req1 = cowboy_req:reply(500, 
                                #{<<"content-type">> => <<"application/json">>}, 
                                Response, Req),
                            {ok, Req1, Opts}
                    end;
                {error, _} ->
                    Response = jiffy:encode(#{
                        <<"success">> => false,
                        <<"error">> => <<"Failed to update balance">>
                    }),
                    Req1 = cowboy_req:reply(500, 
                        #{<<"content-type">> => <<"application/json">>}, 
                        Response, Req),
                    {ok, Req1, Opts}
            end;
        {ok, CurrentBalance} ->
            Response = jiffy:encode(#{
                <<"success">> => false,
                <<"error">> => <<"Insufficient balance">>,
                <<"current_balance">> => CurrentBalance,
                <<"required_amount">> => Amount
            }),
            Req1 = cowboy_req:reply(400, 
                #{<<"content-type">> => <<"application/json">>}, 
                Response, Req),
            {ok, Req1, Opts};
        {error, _} ->
            Response = jiffy:encode(#{
                <<"success">> => false,
                <<"error">> => <<"Failed to get user balance">>
            }),
            Req1 = cowboy_req:reply(500, 
                #{<<"content-type">> => <<"application/json">>}, 
                Response, Req),
            {ok, Req1, Opts}
    end;

process_transaction(sell, UserId, Amount, Req, Opts) ->
    case db_server:get_user_balance(UserId) of
        {ok, CurrentBalance} ->
            NewBalance = CurrentBalance + Amount,
            Timestamp = iso8601_timestamp(),
            
            case db_server:update_balance(UserId, NewBalance) of
                ok ->
                    case db_server:create_transaction(UserId, sell, Amount, Timestamp) of
                        {ok, TransactionId} ->
                            Response = jiffy:encode(#{
                                <<"success">> => true,
                                <<"message">> => <<"Sale successful">>,
                                <<"transaction_id">> => list_to_binary(TransactionId),
                                <<"new_balance">> => NewBalance,
                                <<"amount">> => Amount,
                                <<"type">> => <<"sell">>
                            }),
                            Req1 = cowboy_req:reply(200, 
                                #{<<"content-type">> => <<"application/json">>}, 
                                Response, Req),
                            {ok, Req1, Opts};
                        {error, _} ->
                            % Rollback balance update
                            db_server:update_balance(UserId, CurrentBalance),
                            Response = jiffy:encode(#{
                                <<"success">> => false,
                                <<"error">> => <<"Transaction failed">>
                            }),
                            Req1 = cowboy_req:reply(500, 
                                #{<<"content-type">> => <<"application/json">>}, 
                                Response, Req),
                            {ok, Req1, Opts}
                    end;
                {error, _} ->
                    Response = jiffy:encode(#{
                        <<"success">> => false,
                        <<"error">> => <<"Failed to update balance">>
                    }),
                    Req1 = cowboy_req:reply(500, 
                        #{<<"content-type">> => <<"application/json">>}, 
                        Response, Req),
                    {ok, Req1, Opts}
            end;
        {error, _} ->
            Response = jiffy:encode(#{
                <<"success">> => false,
                <<"error">> => <<"Failed to get user balance">>
            }),
            Req1 = cowboy_req:reply(500, 
                #{<<"content-type">> => <<"application/json">>}, 
                Response, Req),
            {ok, Req1, Opts}
    end.

get_user_from_token(Req) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        <<"Bearer ", Token/binary>> ->
            auth_utils:verify_jwt(Token);
        _ ->
            {error, <<"Missing or invalid authorization header">>}
    end.

validate_amount(Amount) when is_number(Amount), Amount > 0 ->
    ok;
validate_amount(_) ->
    {error, <<"Amount must be a positive number">>}.

iso8601_timestamp() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0wZ", 
                  [Year, Month, Day, Hour, Min, Sec]).
