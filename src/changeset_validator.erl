%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Validator module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(changeset_validator).

-export([validate_change/3, validate_data/3, validate/4]).
-export([validate_is_required/1, validate_is_required/2]).
-export([validator_by_field_type/2]).

-export_type([return/0]).

-include("changeset.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type return() :: #changeset{} | [error()].

-callback validate(field()) -> fun((#changeset{}) -> return()).

validate_change( Validate
               , Field
               , #changeset{changes = Changes} = Changeset ) ->
    validate(Validate, Field, Changes, Changeset).

validate_data( Validate
             , Field
             , #changeset{data = Data} = Changeset ) ->
    validate(Validate, Field, Data, Changeset).

validate(Validate, Field, Data, #changeset{} = Changeset)
    when is_function(Validate, 1)
       , is_map(Data) ->
    do_validate(Field, Data, Validate, Changeset).

do_validate( Field
           , Map
           , Validate
           , #changeset{ default = Default
                       , errors  = Errors } = Changeset )
    when is_map_key(Field, Map) ->
    case proplists:lookup(Field, Errors) of
        {Field, _} ->
            Changeset;
        none ->
            Value = get_field_value(Field, Map, Default),
            case Validate(Value) of
                [] ->
                    Changeset;
                NewErrors when is_list(NewErrors) ->
                    changeset:push_errors(NewErrors, Changeset);
                #changeset{} = NewChangeset ->
                    NewChangeset
            end
    end;
do_validate(_, _, _, Changeset) ->
    Changeset.

validate_is_required(Fields) ->
    fun(Changeset) -> validate_is_required(Fields, Changeset) end.

validate_is_required( [Field | T]
                    , #changeset{empty_values = EmptyValues} = Changeset ) ->
    case is_field_value_truthy(Field, Changeset#changeset.changes, EmptyValues) of
        true ->
            validate_is_required(T, Changeset);
        false ->
            case is_field_value_truthy(Field, Changeset#changeset.data, EmptyValues) of
                true ->
                    validate_is_required(T, Changeset);
                false ->
                    Error = changeset:error( Field
                                           , <<"is required">>
                                           , #{validation => is_required} ),
                    changeset:fold( [ changeset:pop_change(Field)
                                    , changeset:push_error(Error)
                                    ]
                                  , Changeset )
            end
    end;
validate_is_required([], Changeset) ->
    Changeset.

% Field

get_field_value(Field, Map, Default) when is_map(Map) ->
    case maps:find(Field, Map) of
        {ok, Value} ->
            Value;
        error ->
            case Default of
                no_default ->
                    undefined;
                Default when is_function(Default, 0) ->
                    Default()
            end
    end.

is_field_value_truthy(Field, Data, EmptyValues) when is_map(Data) ->
    case maps:find(Field, Data) of
        {ok, Value} ->
            is_truthy(Value, EmptyValues);
        error ->
            false
    end.

validator_by_field_type(atom, Field) ->
    changeset_validate_is_atom:validate(Field);
validator_by_field_type(binary, Field) ->
    changeset_validate_is_binary:validate(Field);
validator_by_field_type(bitstring, Field) ->
    changeset_validate_is_bitstring:validate(Field);
validator_by_field_type(boolean, Field) ->
    changeset_validate_is_boolean:validate(Field);
validator_by_field_type(float, Field) ->
    changeset_validate_is_float:validate(Field);
validator_by_field_type(function, Field) ->
    changeset_validate_is_function:validate(Field);
validator_by_field_type({function, Arity}, Field) ->
    changeset_validate_is_function:validate({Field, Arity});
validator_by_field_type(integer, Field) ->
    changeset_validate_is_integer:validate(Field);
validator_by_field_type(list, Field) ->
    changeset_validate_is_list:validate(Field);
validator_by_field_type(map, Field) ->
    changeset_validate_is_map:validate(Field);
validator_by_field_type(pid, Field) ->
    changeset_validate_is_pid:validate(Field);
validator_by_field_type(port, Field) ->
    changeset_validate_is_port:validate(Field);
validator_by_field_type(record, Field) ->
    changeset_validate_is_record:validate(Field);
validator_by_field_type({record, Name}, Field) ->
    changeset_validate_is_record:validate({Field, Name});
validator_by_field_type({record, Name, Size}, Field) ->
    changeset_validate_is_record:validate({Field, Name, Size});
validator_by_field_type(reference, Field) ->
    changeset_validate_is_reference:validate(Field);
validator_by_field_type(tuple, Field) ->
    changeset_validate_is_tuple:validate(Field).

% Value

is_falsy(Value, EmptyValues) when is_list(EmptyValues) ->
    lists:member(normalize(Value), EmptyValues).

is_truthy(Value, Payload) ->
    not is_falsy(Value, Payload).

normalize(Value) when is_binary(Value) ->
    string:trim(Value);
normalize(Value) ->
    Value.

% Test

-ifdef(TEST).

validate_is_required_test() ->
    Changes = #changeset{ data    = #{}
                        , changes = #{foo => bar}
                        , fields  = [foo] },
    ValidChanges = validate_is_required([foo], Changes),
    InvalidChanges = validate_is_required([bar], Changes),

    Data = #changeset{ data    = #{foo => var}
                     , changes = #{}
                     , fields  = [foo] },
    ValidData = validate_is_required([foo], Data),
    InvalidData = validate_is_required([bar], Data),

    [ { "Should have valid changes"
      , ?assert(ValidChanges#changeset.is_valid)
      }
    , { "Should have invalid changes"
      , ?assertNot(InvalidChanges#changeset.is_valid)
      }
    , { "Should have valid data"
      , ?assert(ValidData#changeset.is_valid)
      }
    , { "Should have invalid data"
      , ?assertNot(InvalidData#changeset.is_valid)
      }
    ].

-endif.
