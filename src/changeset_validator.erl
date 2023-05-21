%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Validator module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(changeset_validator).

-export([ validate_change/2
        , validate_change/3
        , validate_data/2
        , validate_data/3
        , validate/3
        , validate/4
        ]).
-export([validate_is_required/1, validate_is_required/2]).

-include("changeset.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

validate_change(Field, Validate) ->
    fun(#changeset{changes = Changes} = Changeset) ->
        validate(Changeset, Field, Validate, Changes)
    end.

validate_change( #changeset{changes = Changes} = Changeset
               , Field
               , Validate ) ->
    validate(Changeset, Field, Validate, Changes).

validate_data(Field, Validate) ->
    fun(#changeset{data = Data} = Changeset) ->
        validate(Changeset, Field, Validate, Data)
    end.

validate_data( #changeset{data = Data} = Changeset
             , Field
             , Validate ) ->
    validate(Changeset, Field, Validate, Data).

validate(Field, Validate, Payload) ->
    fun(Changeset) ->
        validate(Changeset, Field, Validate, Payload)
    end.

validate(#changeset{} = Changeset, Field, Validate, Payload)
    when is_function(Validate, 1)
       , is_map(Payload) ->
    do_validate(Field, Validate, Payload, Changeset).

do_validate( Field
           , Validate
           , Payload
           , #changeset{ default = Default
                       , errors  = Errors } = Changeset )
    when is_map_key(Field, Payload) ->
    case proplists:lookup(Field, Errors) of
        {Field, _} ->
            Changeset;
        none ->
            Value = get_field_value(Field, Payload, Default),
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
    fun(Changeset) -> validate_is_required(Changeset, Fields) end.

validate_is_required(#changeset{} = Changeset, Fields) when is_list(Fields) ->
    do_validate_is_required(Fields, Changeset#changeset{required = Fields}).

do_validate_is_required( [Field | T]
                       , #changeset{empty_values = EmptyValues} = Changeset ) ->
    case is_field_value_truthy(Field, Changeset#changeset.changes, EmptyValues) of
        true ->
            do_validate_is_required(T, Changeset);
        false ->
            case is_field_value_truthy(Field, Changeset#changeset.data, EmptyValues) of
                true ->
                    do_validate_is_required(T, Changeset);
                false ->
                    Error = changeset:error( Field
                                           , <<"is required">>
                                           , #{validation => is_required} ),
                    changeset:pipe( Changeset
                                  , [ changeset:pop_change(Field)
                                    , changeset:push_error(Error)
                                    ]
                                  )
            end
    end;
do_validate_is_required([], Changeset) ->
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

% Value

is_truthy(Value, Payload) ->
    not is_falsy(Value, Payload).

is_falsy(Value, EmptyValues) when is_list(EmptyValues) ->
    lists:member(normalize(Value), EmptyValues).

normalize(Value) when is_binary(Value) ->
    string:trim(Value);
normalize(Value) ->
    Value.

% Test

-ifdef(TEST).

validate_is_required_test() ->
    ChangesChangeset = #changeset{ data    = #{}
                                 , changes = #{foo => bar}
                                 , fields  = [foo] },
    ValidChanges = validate_is_required(ChangesChangeset, [foo]),
    InvalidChanges = validate_is_required(ChangesChangeset, [bar]),

    DataChangeset = #changeset{ data    = #{foo => var}
                              , changes = #{}
                              , fields  = [foo] },
    ValidData = validate_is_required(DataChangeset, [foo]),
    InvalidData = validate_is_required(DataChangeset, [bar]),

    Required = validate_is_required(#changeset{}, [foo, bar]),

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
    , { "Should have required fields"
      , ?assertEqual([foo, bar], Required#changeset.required)
      }
    ].

-endif.
