%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc List validator module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(changeset_is_list_validator).

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
        (List) when is_list(List) ->
            [];
        (_) ->
            [ changeset:error( Field
                             , <<"must be a list">>
                             , #{validation => is_list} ) ]
    end).

-ifdef(TEST).

validate_change_test() ->
    [ { "Should be valid"
      , ?assert(changeset:is_valid(
            validate_change(foo,
                changeset:cast({#{}, #{foo => list}}, #{foo => []}, [foo])
            )
        ))
      }
    , { "Should be invalid when field is not a list"
      , ?assertNot(changeset:is_valid(
            validate_change(foo,
                changeset:cast({#{}, #{foo => list}}, #{foo => bar}, [foo])
            )
        ))
      }
    ].

-endif.
