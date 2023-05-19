%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Pid validator module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(changeset_validate_is_pid).

-behaviour(changeset_validator).

-export([validate/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

validate(Field) ->
    fun(Changeset) ->
        changeset_validator:validate_change(fun
            (Pid) when is_pid(Pid) ->
                [];
            (_) ->
                [ changeset:error( Field
                                 , <<"must be a pid">>
                                 , #{validation => is_pid} ) ]
        end, Field, Changeset)
    end.

% Test

-ifdef(TEST).

-include("changeset.hrl").

validate_test() ->
    Validate = validate(foo),
    [ { "Should be valid"
      , ?assert(changeset:is_valid(Validate(#changeset{changes = #{foo => list_to_pid("<0.4.1>")}})))
      }
    , { "Should be invalid when field is not a pid"
      , ?assertNot(changeset:is_valid(Validate(#changeset{changes = #{foo => bar}})))
      }
    ].

-endif.