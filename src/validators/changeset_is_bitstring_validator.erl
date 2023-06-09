%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Bitstring validator module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(changeset_is_bitstring_validator).

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
        (Bitstring) when is_bitstring(Bitstring) ->
            [];
        (_) ->
            [ changeset:error( Field
                             , <<"must be a bitstring">>
                             , #{validation => is_bitstring} ) ]
    end).

-ifdef(TEST).

validate_change_test() ->
    [ { "Should be valid"
      , ?assert(changeset:is_valid(
            validate_change(foo,
                changeset:cast({#{}, #{foo => bitstring}}, #{foo => <<>>}, [foo])
            )
        ))
      }
    , { "Should be invalid when field is not a bitstring"
      , ?assertNot(changeset:is_valid(
            validate_change(foo,
                changeset:cast({#{}, #{foo => bitstring}}, #{foo => bar}, [foo])
            )
        ))
      }
    ].

-endif.
