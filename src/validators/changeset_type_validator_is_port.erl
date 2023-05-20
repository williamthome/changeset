%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Port validator module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(changeset_type_validator_is_port).

-behaviour(changeset_type_validator).

-export([validate_change/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

validate_change(Field, Changeset) ->
    changeset_validator:validate_change(Changeset, Field, fun
        (Port) when is_port(Port) ->
            [];
        (_) ->
            [ changeset:error( Field
                             , <<"must be a port">>
                             , #{validation => is_port} ) ]
    end).

% Test

-ifdef(TEST).

-include("changeset.hrl").

validate_change_test() ->
    [ { "Should be valid"
      , ?assert(changeset:is_valid(validate_change(foo, #changeset{changes = #{foo => list_to_port("#Port<0.4>")}})))
      }
    , { "Should be invalid when field is not a port"
      , ?assertNot(changeset:is_valid(validate_change(foo, #changeset{changes = #{foo => bar}})))
      }
    ].

-endif.
