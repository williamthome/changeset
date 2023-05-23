%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Function validator module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(changeset_is_function_validator).

-behaviour(changeset_type_validator).

-export([validate_change/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type changeset() :: changeset:t().
-type field()     :: changeset:field().

-spec validate_change(field(), changeset()) -> changeset().

validate_change({Field, Arity}, Changeset) ->
    changeset:validate_change(Changeset, Field, fun
        (Function) when is_function(Function, Arity) ->
            [];
        (_) ->
            ArityBin = integer_to_binary(Arity),
            [ changeset:error( Field
                             , <<"must be a function of arity ", ArityBin/binary>>
                             , #{ validation => is_function
                                , arity => Arity } ) ]
    end);
validate_change(Field, Changeset) ->
    changeset:validate_change(Changeset, Field, fun
        (Function) when is_function(Function) ->
            [];
        (_) ->
            [ changeset:error( Field
                             , <<"must be a function">>
                             , #{validation => is_function} ) ]
    end).

-ifdef(TEST).

validate_change_test() ->
    [ { "Should be valid"
      , ?assert(changeset:is_valid(
            validate_change(foo,
                changeset:cast({#{}, #{foo => function}}, #{foo => fun() -> bar end}, [foo])
            )
        ))
      }
    , { "Should be invalid when field is not a function"
      , ?assertNot(changeset:is_valid(
            validate_change(foo,
                changeset:cast({#{}, #{foo => function}}, #{foo => bar}, [foo])
            )
        ))
      }
    ].

-endif.
