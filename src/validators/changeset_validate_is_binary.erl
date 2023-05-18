-module(changeset_validate_is_binary).

-behaviour(changeset_validator).

-export([validate/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

validate(Field) ->
    fun(Changeset) ->
        changeset_validator:validate_change(fun
            (Bin) when is_binary(Bin) ->
                [];
            (_) ->
                [ changeset:error( Field
                                 , <<"must be a binary">>
                                 , #{validation => is_binary} ) ]
        end, Field, Changeset)
    end.

% Test

-ifdef(TEST).

-include("changeset.hrl").

validate_test() ->
    Validate = validate(foo),
    [ { "Should be valid"
      , ?assert(changeset:is_valid(Validate(#changeset{changes = #{foo => <<>>}})))
      }
    , { "Should be invalid when field is not a binary"
      , ?assertNot(changeset:is_valid(Validate(#changeset{changes = #{foo => bar}})))
      }
    , { "Should be invalid when field is missing"
      , ?assertNot(changeset:is_valid(Validate(#changeset{changes = #{}})))
      }
    ].

-endif.
