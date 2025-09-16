-module(register_handler).
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
        
        % Validate input
        case validate_registration_data(Username, Password) of
            ok ->
                PasswordHash = auth_utils:hash_password(binary_to_list(Password)),
                case db_server:create_user(binary_to_list(Username), PasswordHash) of
                    {ok, UserId} ->
                        Response = jiffy:encode(#{
                            <<"success">> => true,
                            <<"message">> => <<"User created successfully">>,
                            <<"user_id">> => list_to_binary(UserId)
                        }),
                        Req2 = cowboy_req:reply(201, 
                            #{<<"content-type">> => <<"application/json">>}, 
                            Response, Req1),
                        {ok, Req2, Opts};
                    {error, username_exists} ->
                        Response = jiffy:encode(#{
                            <<"success">> => false,
                            <<"error">> => <<"Username already exists">>
                        }),
                        Req2 = cowboy_req:reply(409, 
                            #{<<"content-type">> => <<"application/json">>}, 
                            Response, Req1),
                        {ok, Req2, Opts};
                    {error, _Reason} ->
                        ResponseError = jiffy:encode(#{
                            <<"success">> => false,
                            <<"error">> => <<"Database error">>
                        }),
                        ReqError = cowboy_req:reply(500, 
                            #{<<"content-type">> => <<"application/json">>}, 
                            ResponseError, Req1),
                        {ok, ReqError, Opts}
                end;
            {error, Reason} ->
                ResponseValidation = jiffy:encode(#{
                    <<"success">> => false,
                    <<"error">> => Reason
                }),
                ReqValidation = cowboy_req:reply(400, 
                    #{<<"content-type">> => <<"application/json">>}, 
                    ResponseValidation, Req1),
                {ok, ReqValidation, Opts}
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

handle_request(_, Req0, Opts) ->
    Response = jiffy:encode(#{
        <<"success">> => false,
        <<"error">> => <<"Method not allowed">>
    }),
    Req1 = cowboy_req:reply(405, 
        #{<<"content-type">> => <<"application/json">>}, 
        Response, Req0),
    {ok, Req1, Opts}.

validate_registration_data(Username, Password) ->
    case {Username, Password} of
        {<<>>, _} -> {error, <<"Username cannot be empty">>};
        {_, <<>>} -> {error, <<"Password cannot be empty">>};
        {U, _P} when byte_size(U) < 3 -> {error, <<"Username must be at least 3 characters">>};
        {_U, P} when byte_size(P) < 6 -> {error, <<"Password must be at least 6 characters">>};
        {U, _P} when byte_size(U) > 50 -> {error, <<"Username too long">>};
        _ -> ok
    end.
