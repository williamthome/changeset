%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Port validator module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(changeset_validate_is_port).

-behaviour(changeset_validator).

-export([validate/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

validate(Field) ->
    changeset_validator:validate_change(fun
        (Port) when is_port(Port) ->
            [];
        (_) ->
            [ changeset:error( Field
                             , <<"must be a port">>
                             , #{validation => is_port} ) ]
    end, Field).

% Test

-ifdef(TEST).

-include("changeset.hrl").

validate_test() ->
    Validate = validate(foo),
    [ { "Should be valid"
      , ?assert(changeset:is_valid(Validate(#changeset{changes = #{foo => list_to_port("#Port<0.4>")}})))
      }
    , { "Should be invalid when field is not a port"
      , ?assertNot(changeset:is_valid(Validate(#changeset{changes = #{foo => bar}})))
      }
    ].

-endif.
