-module(balance_handler).
-export([init/2]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    handle_request(Method, Req0, Opts).

handle_request(<<"GET">>, Req0, Opts) ->
    case get_user_from_token(Req0) of
        {ok, UserId} ->
            case db_server:get_user_balance(UserId) of
                {ok, Balance} ->
                    Response = jiffy:encode(#{
                        <<"success">> => true,
                        <<"balance">> => Balance,
                        <<"user_id">> => list_to_binary(UserId)
                    }),
                    Req1 = cowboy_req:reply(200, 
                        #{<<"content-type">> => <<"application/json">>}, 
                        Response, Req0),
                    {ok, Req1, Opts};
                {error, user_not_found} ->
                    Response = jiffy:encode(#{
                        <<"success">> => false,
                        <<"error">> => <<"User not found">>
                    }),
                    Req1 = cowboy_req:reply(404, 
                        #{<<"content-type">> => <<"application/json">>}, 
                        Response, Req0),
                    {ok, Req1, Opts};
                {error, _} ->
                    Response = jiffy:encode(#{
                        <<"success">> => false,
                        <<"error">> => <<"Database error">>
                    }),
                    Req1 = cowboy_req:reply(500, 
                        #{<<"content-type">> => <<"application/json">>}, 
                        Response, Req0),
                    {ok, Req1, Opts}
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
    end;

handle_request(_, Req0, Opts) ->
    Response = jiffy:encode(#{
        <<"success">> => false,
        <<"error">> => <<"Method not allowed">>
    }),
    Req1 = cowboy_req:reply(405, 
        #{<<"content-type">> => <<"application/json">>}, 
        Response, Req0),
    {ok, Req1, Opts}.

get_user_from_token(Req) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        <<"Bearer ", Token/binary>> ->
            auth_utils:verify_jwt(Token);
        _ ->
            {error, <<"Missing or invalid authorization header">>}
    end.
