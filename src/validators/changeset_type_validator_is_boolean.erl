%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Boolean validator module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(changeset_type_validator_is_boolean).

-behaviour(changeset_type_validator).

-export([validate_change/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

validate_change(Field, Changeset) ->
    changeset_validator:validate_change(Changeset, Field, fun
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

validate_change_test() ->
    [ { "Should be valid"
      , ?assert(changeset:is_valid(validate_change(foo, #changeset{changes = #{foo => true}})))
      }
    , { "Should be invalid when field is not a boolean"
      , ?assertNot(changeset:is_valid(validate_change(foo, #changeset{changes = #{foo => bar}})))
      }
    ].

-endif.
