%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Atom validator module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(changeset_is_atom_validator).

-behaviour(changeset_type_validator).

-export([validate_change/2]).

-include("changeset.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec validate_change(field(), changeset()) -> changeset().

validate_change(Field, Changeset) ->
    changeset_validator:validate_change(Changeset, Field, fun
        (Atom) when is_atom(Atom) ->
            [];
        (_) ->
            [ changeset:error( Field
                             , <<"must be an atom">>
                             , #{validation => is_atom} ) ]
    end).

-ifdef(TEST).

validate_change_test() ->
    [ { "Should be valid"
      , ?assert(changeset:is_valid(
            validate_change(foo, #changeset{changes = #{foo => bar}})
        ))
      }
    , { "Should be invalid when field is not an atom"
      , ?assertNot(changeset:is_valid(
            validate_change(foo, #changeset{changes = #{foo => <<>>}})
        ))
      }
    ].

-endif.
