%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Atom validator module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(changeset_is_atom_validator).

-behaviour(changeset_type_validator).

-export([validate_change/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type changeset() :: changeset:t().
-type field()     :: changeset:field().

-spec validate_change(field(), changeset()) -> changeset().

validate_change(Field, Changeset) ->
    changeset:validate_change(Changeset, Field, fun
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
            validate_change(foo,
                changeset:cast({#{}, #{foo => atom}}, #{foo => bar}, [foo])
            )
        ))
      }
    , { "Should be invalid when field is not an atom"
      , ?assertNot(changeset:is_valid(
            validate_change(foo,
                changeset:cast({#{}, #{foo => atom}}, #{foo => <<>>}, [foo])
            )
        ))
      }
    ].

-endif.
