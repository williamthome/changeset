%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Pid validator module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(changeset_is_pid_validator).

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
        (Pid) when is_pid(Pid) ->
            [];
        (_) ->
            [ changeset:error( Field
                             , <<"must be a pid">>
                             , #{validation => is_pid} ) ]
    end).

-ifdef(TEST).

validate_change_test() ->
    [ { "Should be valid"
      , ?assert(changeset:is_valid(
            validate_change(foo,
                changeset:cast({#{}, #{foo => pid}}, #{foo => list_to_pid("<0.4.1>")}, [foo])
            )
        ))
      }
    , { "Should be invalid when field is not a pid"
      , ?assertNot(changeset:is_valid(
            validate_change(foo,
                changeset:cast({#{}, #{foo => pid}}, #{foo => bar}, [foo])
            )
        ))
      }
    ].

-endif.
