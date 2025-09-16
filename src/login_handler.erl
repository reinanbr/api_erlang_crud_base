-module(login_handler).
-export([init/2]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    handle_request(Method, Req0, Opts).

handle_request(<<"POST">>, Req0, Opts) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    try
        Json = jiffy:decode(Body, [return_maps]),
        Username = maps:get(<<"username">>, Json),
        Password = maps:get(<<"password">>, Json),
        
        case db_server:authenticate_user(binary_to_list(Username), binary_to_list(Password)) of
            {ok, UserId} ->
                case auth_utils:generate_jwt(UserId) of
                    {ok, Token} ->
                        Response = jiffy:encode(#{
                            <<"success">> => true,
                            <<"message">> => <<"Login successful">>,
                            <<"token">> => Token,
                            <<"user_id">> => list_to_binary(UserId)
                        }),
                        Req2 = cowboy_req:reply(200, 
                            #{<<"content-type">> => <<"application/json">>}, 
                            Response, Req1),
                        {ok, Req2, Opts};
                    {error, _} ->
                        Response2 = jiffy:encode(#{
                            <<"success">> => false,
                            <<"error">> => <<"Failed to generate token">>
                        }),
                        Req2 = cowboy_req:reply(500, 
                            #{<<"content-type">> => <<"application/json">>}, 
                            Response2, Req1),
                        {ok, Req2, Opts}
                end;
            {error, invalid_credentials} ->
                Response3 = jiffy:encode(#{
                    <<"success">> => false,
                    <<"error">> => <<"Invalid username or password">>
                }),
                Req2 = cowboy_req:reply(401, 
                    #{<<"content-type">> => <<"application/json">>}, 
                    Response3, Req1),
                {ok, Req2, Opts};
            {error, user_not_found} ->
                Response4 = jiffy:encode(#{
                    <<"success">> => false,
                    <<"error">> => <<"Invalid username or password">>
                }),
                Req2 = cowboy_req:reply(401, 
                    #{<<"content-type">> => <<"application/json">>}, 
                    Response4, Req1),
                {ok, Req2, Opts};
            {error, _} ->
                Response5 = jiffy:encode(#{
                    <<"success">> => false,
                    <<"error">> => <<"Authentication failed">>
                }),
                Req2 = cowboy_req:reply(500, 
                    #{<<"content-type">> => <<"application/json">>}, 
                    Response5, Req1),
                {ok, Req2, Opts}
        end
    catch
        _:_ ->
            ResponseError = jiffy:encode(#{
                <<"success">> => false,
                <<"error">> => <<"Invalid JSON">>
            }),
            ReqError = cowboy_req:reply(400, 
                #{<<"content-type">> => <<"application/json">>}, 
                ResponseError, Req1),
            {ok, ReqError, Opts}
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
