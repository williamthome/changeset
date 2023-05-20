%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Reference validator module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(changeset_validate_is_reference).

-behaviour(changeset_validator).

-export([validate/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

validate(Field) ->
    changeset_validator:validate_change(Field, fun
        (Reference) when is_reference(Reference) ->
            [];
        (_) ->
            [ changeset:error( Field
                             , <<"must be a reference">>
                             , #{validation => is_reference} ) ]
    end).

% Test

-ifdef(TEST).

-include("changeset.hrl").

validate_test() ->
    Validate = validate(foo),
    [ { "Should be valid"
      , ?assert(changeset:is_valid(Validate(#changeset{changes = #{foo => list_to_ref("#Ref<0.4192537678.4073193475.71181>")}})))
      }
    , { "Should be invalid when field is not a reference"
      , ?assertNot(changeset:is_valid(Validate(#changeset{changes = #{foo => bar}})))
      }
    ].

-endif.
