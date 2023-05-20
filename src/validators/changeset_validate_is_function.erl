%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Function validator module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(changeset_validate_is_function).

-behaviour(changeset_validator).

-export([validate/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

validate({Field, Arity}) ->
    changeset_validator:validate_change(Field, fun
        (Function) when is_function(Function, Arity) ->
            [];
        (_) ->
            ArityBin = integer_to_binary(Arity),
            [ changeset:error( Field
                             , <<"must be a function of arity ", ArityBin/binary>>
                             , #{ validation => is_function
                                , arity => Arity } ) ]
    end);
validate(Field) ->
    changeset_validator:validate_change(Field, fun
        (Function) when is_function(Function) ->
            [];
        (_) ->
            [ changeset:error( Field
                             , <<"must be a function">>
                             , #{validation => is_function} ) ]
    end).

% Test

-ifdef(TEST).

-include("changeset.hrl").

validate_test() ->
    Validate = validate(foo),
    [ { "Should be valid"
      , ?assert(changeset:is_valid(Validate(#changeset{changes = #{foo => fun() -> bar end}})))
      }
    , { "Should be invalid when field is not a function"
      , ?assertNot(changeset:is_valid(Validate(#changeset{changes = #{foo => bar}})))
      }
    ].

-endif.
