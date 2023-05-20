%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Pid validator module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(changeset_pid_validator).

-behaviour(changeset_type_validator).

-export([validate_change/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

validate_change(Field, Changeset) ->
    changeset_validator:validate_change(Changeset, Field, fun
        (Pid) when is_pid(Pid) ->
            [];
        (_) ->
            [ changeset:error( Field
                             , <<"must be a pid">>
                             , #{validation => is_pid} ) ]
    end).

% Test

-ifdef(TEST).

-include("changeset.hrl").

validate_change_test() ->
    [ { "Should be valid"
      , ?assert(changeset:is_valid(validate_change(foo, #changeset{changes = #{foo => list_to_pid("<0.4.1>")}})))
      }
    , { "Should be invalid when field is not a pid"
      , ?assertNot(changeset:is_valid(validate_change(foo, #changeset{changes = #{foo => bar}})))
      }
    ].

-endif.
