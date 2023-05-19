%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Atom validator module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(changeset_validate_is_atom).

-behaviour(changeset_validator).

-export([validate/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

validate(Field) ->
    fun(Changeset) ->
        changeset_validator:validate_change(fun
            (Atom) when is_atom(Atom) ->
                [];
            (_) ->
                [ changeset:error( Field
                                 , <<"must be an atom">>
                                 , #{validation => is_atom} ) ]
        end, Field, Changeset)
    end.

% Test

-ifdef(TEST).

-include("changeset.hrl").

validate_test() ->
    Validate = validate(foo),
    [ { "Should be valid"
      , ?assert(changeset:is_valid(Validate(#changeset{changes = #{foo => bar}})))
      }
    , { "Should be invalid when field is not an atom"
      , ?assertNot(changeset:is_valid(Validate(#changeset{changes = #{foo => <<>>}})))
      }
    ].

-endif.
