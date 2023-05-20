%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Atom validator module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(changeset_atom_validator).

-behaviour(changeset_type_validator).

-export([validate_change/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

validate_change(Field, Changeset) ->
    changeset_validator:validate_change(Changeset, Field, fun
        (Atom) when is_atom(Atom) ->
            [];
        (_) ->
            [ changeset:error( Field
                             , <<"must be an atom">>
                             , #{validation => is_atom} ) ]
    end).

% Test

-ifdef(TEST).

-include("changeset.hrl").

validate_change_test() ->
    [ { "Should be valid"
      , ?assert(changeset:is_valid(validate_change(foo, #changeset{changes = #{foo => bar}})))
      }
    , { "Should be invalid when field is not an atom"
      , ?assertNot(changeset:is_valid(validate_change(foo, #changeset{changes = #{foo => <<>>}})))
      }
    ].

-endif.
