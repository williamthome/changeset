%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Integer validator module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(changeset_validate_is_integer).

-behaviour(changeset_validator).

-export([validate/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

validate(Field) ->
    changeset_validator:validate_change(fun
        (Integer) when is_integer(Integer) ->
            [];
        (_) ->
            [ changeset:error( Field
                             , <<"must be an integer">>
                             , #{validation => is_integer} ) ]
    end, Field).

% Test

-ifdef(TEST).

-include("changeset.hrl").

validate_test() ->
    Validate = validate(foo),
    [ { "Should be valid"
      , ?assert(changeset:is_valid(Validate(#changeset{changes = #{foo => 0}})))
      }
    , { "Should be invalid when field is not an integer"
      , ?assertNot(changeset:is_valid(Validate(#changeset{changes = #{foo => bar}})))
      }
    ].

-endif.
