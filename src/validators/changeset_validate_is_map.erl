%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Map validator module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(changeset_validate_is_map).

-behaviour(changeset_validator).

-export([validate/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

validate(Field) ->
    fun(Changeset) ->
        changeset_validator:validate_change(fun
            (Map) when is_map(Map) ->
                [];
            (_) ->
                [ changeset:error( Field
                                 , <<"must be a map">>
                                 , #{validation => is_map} ) ]
        end, Field, Changeset)
    end.

% Test

-ifdef(TEST).

-include("changeset.hrl").

validate_test() ->
    Validate = validate(foo),
    [ { "Should be valid"
      , ?assert(changeset:is_valid(Validate(#changeset{changes = #{foo => #{}}})))
      }
    , { "Should be invalid when field is not a map"
      , ?assertNot(changeset:is_valid(Validate(#changeset{changes = #{foo => bar}})))
      }
    ].

-endif.
