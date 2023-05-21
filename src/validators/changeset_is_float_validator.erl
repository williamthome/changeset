%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Float validator module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(changeset_is_float_validator).

-behaviour(changeset_type_validator).

-export([validate_change/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

validate_change(Field, Changeset) ->
    changeset_validator:validate_change(Changeset, Field, fun
        (Float) when is_float(Float) ->
            [];
        (_) ->
            [ changeset:error( Field
                             , <<"must be a float">>
                             , #{validation => is_float} ) ]
    end).

% Test

-ifdef(TEST).

-include("changeset.hrl").

validate_change_test() ->
    [ { "Should be valid"
      , ?assert(changeset:is_valid(
            validate_change(foo, #changeset{changes = #{foo => 0.0}})
        ))
      }
    , { "Should be invalid when field is not a float"
      , ?assertNot(changeset:is_valid(
            validate_change(foo, #changeset{changes = #{foo => bar}})
        ))
      }
    ].

-endif.
