%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Boolean validator module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(changeset_validate_is_boolean).

-behaviour(changeset_validator).

-export([validate/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

validate(Field) ->
    changeset_validator:validate_change(Field, fun
        (Boolean) when is_boolean(Boolean) ->
            [];
        (_) ->
            [ changeset:error( Field
                             , <<"must be a boolean">>
                             , #{validation => is_boolean} ) ]
    end).

% Test

-ifdef(TEST).

-include("changeset.hrl").

validate_test() ->
    Validate = validate(foo),
    [ { "Should be valid"
      , ?assert(changeset:is_valid(Validate(#changeset{changes = #{foo => true}})))
      }
    , { "Should be invalid when field is not a boolean"
      , ?assertNot(changeset:is_valid(Validate(#changeset{changes = #{foo => bar}})))
      }
    ].

-endif.
