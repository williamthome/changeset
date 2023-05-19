%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Bitstring validator module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(changeset_validate_is_bitstring).

-behaviour(changeset_validator).

-export([validate/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

validate(Field) ->
    fun(Changeset) ->
        changeset_validator:validate_change(fun
            (Bitstring) when is_bitstring(Bitstring) ->
                [];
            (_) ->
                [ changeset:error( Field
                                 , <<"must be a bitstring">>
                                 , #{validation => is_bitstring} ) ]
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
    , { "Should be invalid when field is not a bitstring"
      , ?assertNot(changeset:is_valid(Validate(#changeset{changes = #{foo => bar}})))
      }
    ].

-endif.
