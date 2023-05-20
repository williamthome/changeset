%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Tuple validator module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(changeset_tuple_validator).

-behaviour(changeset_type_validator).

-export([validate_change/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

validate_change(Field, Changeset) ->
    changeset_validator:validate_change(Changeset, Field, fun
        (Tuple) when is_tuple(Tuple) ->
            [];
        (_) ->
            [ changeset:error( Field
                             , <<"must be a tuple">>
                             , #{validation => is_tuple} ) ]
    end).

% Test

-ifdef(TEST).

-include("changeset.hrl").

validate_change_test() ->
    [ { "Should be valid"
      , ?assert(changeset:is_valid(validate_change(foo, #changeset{changes = #{foo => {}}})))
      }
    , { "Should be invalid when field is not a tuple"
      , ?assertNot(changeset:is_valid(validate_change(foo, #changeset{changes = #{foo => bar}})))
      }
    ].

-endif.
