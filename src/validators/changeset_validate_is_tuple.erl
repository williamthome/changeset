%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Tuple validator module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(changeset_validate_is_tuple).

-behaviour(changeset_validator).

-export([validate/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

validate(Field) ->
    changeset_validator:validate_change(Field, fun
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

validate_test() ->
    Validate = validate(foo),
    [ { "Should be valid"
      , ?assert(changeset:is_valid(Validate(#changeset{changes = #{foo => {}}})))
      }
    , { "Should be invalid when field is not a tuple"
      , ?assertNot(changeset:is_valid(Validate(#changeset{changes = #{foo => bar}})))
      }
    ].

-endif.
