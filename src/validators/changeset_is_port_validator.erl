%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Port validator module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(changeset_is_port_validator).

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
        (Port) when is_port(Port) ->
            [];
        (_) ->
            [ changeset:error( Field
                             , <<"must be a port">>
                             , #{validation => is_port} ) ]
    end).

-ifdef(TEST).

validate_change_test() ->
    [ { "Should be valid"
      , ?assert(changeset:is_valid(
            validate_change(foo,
                changeset:cast({#{}, #{foo => port}}, #{foo => list_to_port("#Port<0.4>")}, [foo])
            )
        ))
      }
    , { "Should be invalid when field is not a port"
      , ?assertNot(changeset:is_valid(
            validate_change(foo,
                changeset:cast({#{}, #{foo => port}}, #{foo => bar}, [foo])
            )
        ))
      }
    ].

-endif.
