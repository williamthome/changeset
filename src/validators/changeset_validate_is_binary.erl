%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Binary validator module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(changeset_validate_is_binary).

-behaviour(changeset_validator).

-export([validate/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

validate(Field) ->
    changeset_validator:validate_change(fun
        (Binary) when is_binary(Binary) ->
            [];
        (_) ->
            [ changeset:error( Field
                             , <<"must be a binary">>
                             , #{validation => is_binary} ) ]
    end, Field).

% Test

-ifdef(TEST).

-include("changeset.hrl").

validate_test() ->
    Validate = validate(foo),
    [ { "Should be valid"
      , ?assert(changeset:is_valid(Validate(#changeset{changes = #{foo => <<>>}})))
      }
      % TODO: Move missing field test to validator module
    , { "Should be valid when field is missing"
      , ?assert(changeset:is_valid(Validate(#changeset{changes = #{}})))
      }
    , { "Should be invalid when field is not a binary"
      , ?assertNot(changeset:is_valid(Validate(#changeset{changes = #{foo => bar}})))
      }
    ].

-endif.
