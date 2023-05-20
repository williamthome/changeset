%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Float validator module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(changeset_validate_is_float).

-behaviour(changeset_validator).

-export([validate/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

validate(Field) ->
    changeset_validator:validate_change(fun
        (Float) when is_float(Float) ->
            [];
        (_) ->
            [ changeset:error( Field
                             , <<"must be a float">>
                             , #{validation => is_float} ) ]
    end, Field).

% Test

-ifdef(TEST).

-include("changeset.hrl").

validate_test() ->
    Validate = validate(foo),
    [ { "Should be valid"
      , ?assert(changeset:is_valid(Validate(#changeset{changes = #{foo => 0.0}})))
      }
    , { "Should be invalid when field is not a float"
      , ?assertNot(changeset:is_valid(Validate(#changeset{changes = #{foo => bar}})))
      }
    ].

-endif.
