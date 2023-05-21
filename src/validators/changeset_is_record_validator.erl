%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Record validator module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(changeset_is_record_validator).

-behaviour(changeset_type_validator).

-export([validate_change/2]).

-include("changeset.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec validate_change(field(), changeset()) -> changeset().

validate_change({Field, Name}, Changeset) ->
    changeset_validator:validate_change(Changeset, Field, fun
        (Change) ->
            case is_record(Change, Name) of
                true ->
                    [];
                false ->
                    NameBin = atom_to_binary(Name),
                    [ changeset:error( Field
                                     , <<"must be a record of name ", NameBin/binary>>
                                     , #{ validation => is_record
                                        , record => Name } ) ]
            end
    end);
validate_change({Field, Name, Size}, Changeset) ->
    changeset_validator:validate_change(Changeset, Field, fun
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
                                        , size => Size } ) ]
            end
    end);
validate_change(Field, Changeset) when is_atom(Field) ->
    validate_change({Field, Field}, Changeset).

-ifdef(TEST).

-record(foo, {}).

validate_change_test() ->
    [ { "Should be valid"
      , ?assert(changeset:is_valid(
            validate_change(foo, #changeset{changes = #{foo => #foo{}}})
        ))
      }
    , { "Should be invalid when field is not a record"
      , ?assertNot(changeset:is_valid(
            validate_change(foo, #changeset{changes = #{foo => bar}})
        ))
      }
    ].

-endif.
