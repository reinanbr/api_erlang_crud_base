-module(auth_utils).
-export([hash_password/1, verify_password/2, generate_jwt/1, verify_jwt/1]).

%% Hash password using simple SHA256
hash_password(Password) ->
    crypto:hash(sha256, Password).

%% Verify password against hash
verify_password(Password, Hash) ->
    Hash =:= hash_password(Password).

%% Generate JWT token for user
generate_jwt(UserId) ->
    Header = #{<<"alg">> => <<"HS256">>, <<"typ">> => <<"JWT">>},
    Claims = #{
        <<"sub">> => list_to_binary(UserId),
        <<"iat">> => erlang:system_time(second),
        <<"exp">> => erlang:system_time(second) + 3600  % Expires in 1 hour
    },
    Secret = get_jwt_secret(),
    case jose_jwt:sign(jose_jwk:from_oct(Secret), Header, Claims) of
        {_Jws, Token} -> {ok, Token};
        Error -> {error, Error}
    end.

%% Verify JWT token and extract user ID
verify_jwt(Token) ->
    Secret = get_jwt_secret(),
    try
        {true, Claims, _Jws} = jose_jwt:verify(jose_jwk:from_oct(Secret), Token),
        #{<<"sub">> := UserId, <<"exp">> := Exp} = Claims,
        CurrentTime = erlang:system_time(second),
        case Exp > CurrentTime of
            true -> {ok, binary_to_list(UserId)};
            false -> {error, token_expired}
        end
    catch
        _:_ -> {error, invalid_token}
    end.

%% Internal function to get JWT secret
get_jwt_secret() ->
    % In production, this should come from environment or config
    <<"your_super_secret_jwt_key_here">>.
