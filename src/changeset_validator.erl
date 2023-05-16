-module(changeset_validator).

-export([validate_change/3, validate_data/3, validate/4]).
-export([validate_is_required/1, validate_is_required/2]).
-export([push_error/1, push_error/2]).
-export([push_errors/1, push_errors/2]).
-export([push_change/2, push_change/3]).
-export([push_changes/1, push_changes/2]).
-export([pop_change/1, pop_change/2]).
-export([pop_changes/1, pop_changes/2]).

-include("changeset.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

validate_change( Validate
               , Field
               , #changeset{changes = Changes} = Changeset ) ->
    validate(Validate, Field, Changeset, Changes).

validate_data( Validate
             , Field
             , #changeset{data = Data} = Changeset ) ->
    validate(Validate, Field, Changeset, Data).

validate( Validate
        , Field
        , #changeset{ default_value = Default
                    , errors  = Errors } = Changeset
        , Map ) when is_function(Validate, 1) ->
    case proplists:lookup(Field, Errors) of
        {Field, _} ->
            Changeset;
        none ->
            Value = get_field_value(Field, Map, Default),
            case Validate(Value) of
                ok ->
                    Changeset;
                {ok, #changeset{} = NewChangeset} ->
                    NewChangeset;
                {error, NewErrors} when is_list(NewErrors) ->
                    push_errors(NewErrors, Changeset);
                {error, Error} ->
                    push_error(Error, Changeset)
            end
    end.

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
                    Error = {Field, { <<"is required">>
                                    , [{validation, required}] }},
                    fold([ pop_change(Field), push_error(Error) ], Changeset)
            end
    end;
validate_is_required([], Changeset) ->
    Changeset.

% Changeset

fold(Funs, Changeset) ->
    lists:foldl(fun(F, CSet) -> F(CSet) end, Changeset, Funs).

% Field

get_field_value(Field, Map, Default) when is_map(Map), is_function(Default, 0) ->
    case maps:find(Field, Map) of
        {ok, Value} ->
            Value;
        error ->
            Default()
    end.

is_field_value_truthy(Field, Data, EmptyValues) when is_map(Data) ->
    case maps:find(Field, Data) of
        {ok, Value} ->
            is_truthy(Value, EmptyValues);
        error ->
            false
    end.

% Value

is_falsy(Value, EmptyValues) when is_list(EmptyValues) ->
    lists:member(normalize(Value), EmptyValues).

is_truthy(Value, Payload) ->
    not is_falsy(Value, Payload).

normalize(Value) when is_binary(Value) ->
    string:trim(Value);
normalize(Value) ->
    Value.

% Error

push_error(Error, #changeset{errors = Errors} = Changeset) ->
    Changeset#changeset{ errors = [Error | Errors]
                       , is_valid = false }.

push_error(Error) ->
    fun(Changeset) -> push_error(Error, Changeset) end.

push_errors(Errors, Changeset) ->
    lists:foldl( fun(Err, CSet) -> push_error(Err, CSet) end
               , Changeset
               , Errors ).

push_errors(Errors) ->
    fun(Changeset) -> push_errors(Errors, Changeset) end.

% Change

push_change( Field
           , Value
           , #changeset{changes = Changes} = Changeset ) ->
    Changeset#changeset{changes = Changes#{Field => Value}}.

push_change(Field, Value) ->
    fun(Changeset) -> push_change(Field, Value, Changeset) end.

push_changes(Changes, Changeset) when is_list(Changes) ->
    lists:foldl( fun({Field, Value}, CSet) -> push_change(Field, Value, CSet) end
               , Changeset
               , Changes );
push_changes( Changes
            , #changeset{changes = CurrChanges} = Changeset ) when is_map(Changes) ->
    Changeset#changeset{changes = maps:merge(CurrChanges, Changes)}.

push_changes(Changes) ->
    fun(Changeset) -> push_changes(Changeset, Changes) end.

pop_change(Field, Changeset) ->
    pop_changes([Field], Changeset).

pop_change(Field) ->
    fun(Changeset) -> pop_change(Field, Changeset) end.

pop_changes( Fields
           , #changeset{changes = Changes} = Changeset ) when is_list(Fields) ->
    Changeset#changeset{changes = maps:without(Fields, Changes)}.

pop_changes(Fields) ->
    fun(Changeset) -> pop_changes(Fields, Changeset) end.

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
      , ?assert(ValidChanges#changeset.is_valid) },
      { "Should have invalid changes"
      , ?assertNot(InvalidChanges#changeset.is_valid) },
      { "Should have valid data"
      , ?assert(ValidData#changeset.is_valid) },
      { "Should have invalid data"
      , ?assertNot(InvalidData#changeset.is_valid) }
    ].

-endif.
