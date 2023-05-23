%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Integer validator module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(changeset_is_integer_validator).

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
        (Integer) when is_integer(Integer) ->
            [];
        (_) ->
            [ changeset:error( Field
                             , <<"must be an integer">>
                             , #{validation => is_integer} ) ]
    end).

-ifdef(TEST).

validate_change_test() ->
    [ { "Should be valid"
      , ?assert(changeset:is_valid(
            validate_change(foo,
                changeset:cast({#{}, #{foo => integer}}, #{foo => 0}, [foo])
            )
        ))
      }
    , { "Should be invalid when field is not an integer"
      , ?assertNot(changeset:is_valid(
            validate_change(foo,
                changeset:cast({#{}, #{foo => integer}}, #{foo => bar}, [foo])
            )
        ))
      }
    ].

-endif.
