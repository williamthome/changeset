%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Record validator module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(changeset_validate_is_record).

-behaviour(changeset_validator).

-export([validate/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

validate({Field, Name}) ->
    fun(Changeset) ->
        changeset_validator:validate_change(fun
            (Change) ->
                case is_record(Change, Name) of
                    true ->
                        [];
                    false ->
                        NameBin = atom_to_binary(Name),
                        [ changeset:error( Field
                                        , <<"must be a record of name ", NameBin/binary>>
                                        , #{ validation => is_record
                                            , record => Name
                                            } ) ]
                end
        end, Field, Changeset)
    end;
validate({Field, Name, Size}) ->
    fun(Changeset) ->
        changeset_validator:validate_change(fun
            (Change) ->
                case is_record(Change, Name, Size) of
                    true ->
                        [];
                    false ->
                        NameBin = atom_to_binary(Name),
                        SizeBin = integer_to_binary(Size),
                        [ changeset:error( Field
                                        , <<"must be a record of name ", NameBin/binary, " and size ", SizeBin/binary>>
                                        , #{ validation => is_record
                                            , record => Name
                                            , size => Size
                                            } ) ]
                end
        end, Field, Changeset)
    end;
validate(Field) when is_atom(Field) ->
    validate({Field, Field}).

% Test

-ifdef(TEST).

-include("changeset.hrl").

-record(bar, {}).

validate_test() ->
    Validate = validate({foo, bar}),
    [ { "Should be valid"
      , ?assert(changeset:is_valid(Validate(#changeset{changes = #{foo => #bar{}}})))
      }
    , { "Should be invalid when field is not a record"
      , ?assertNot(changeset:is_valid(Validate(#changeset{changes = #{foo => bar}})))
      }
    ].

-endif.
