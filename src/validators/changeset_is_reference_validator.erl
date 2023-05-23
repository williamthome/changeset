%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Reference validator module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(changeset_is_reference_validator).

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
        (Reference) when is_reference(Reference) ->
            [];
        (_) ->
            [ changeset:error( Field
                             , <<"must be a reference">>
                             , #{validation => is_reference} ) ]
    end).

-ifdef(TEST).

validate_change_test() ->
    [ { "Should be valid"
      , ?assert(changeset:is_valid(
            validate_change(foo,
                changeset:cast({#{}, #{foo => reference}}, #{foo => make_ref()}, [foo])
            )
        ))
      }
    , { "Should be invalid when field is not a reference"
      , ?assertNot(changeset:is_valid(
            validate_change(foo,
              changeset:cast({#{}, #{foo => reference}}, #{foo => bar}, [foo])
            )
        ))
      }
    ].

-endif.
