%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Changeset module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(changeset).

-export([ get_fields/1
        , get_data/1
        , get_changes/1
        , get_errors/1
        , get_errors/2
        , find_data/2
        , get_data/2
        , get_data/3
        , find_change/2
        , get_change/2
        , get_change/3
        , get_types/1
        , get_type/2
        , get_empty_values/1
        , get_required/1
        , set_required/2
        , is_valid/1
        , is_invalid/1
        , changed/2
        , changed_from/3
        , changed_to/3
        , cast/3
        , cast/4
        , pipe/2
        , error/3
        , push_error/1
        , push_error/2
        , push_errors/1
        , push_errors/2
        , push_change/2
        , push_change/3
        , push_changes/1
        , push_changes/2
        , pop_change/1
        , pop_change/2
        , pop_changes/1
        , pop_changes/2
        , is_field_value_defined/4
        , validate_change/2
        , validate_change/3
        , validate_data/2
        , validate_data/3
        , validate/3
        , validate/4
        , validate_required/1
        , validate_format/2
        , validate_format/4
        , validate_member/2
        , validate_not_member/2
        ]).

-export_type([ t/0
             , field/0
             , type/0
             , cast_opts/0
             , error/0
             , field_error/0
             , pipe/0
             , validation/0
             ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(changeset,
    { fields       = []                :: [field()]
    , types        = #{}               :: #{field() := type()}
    , required     = []                :: [field()]
    , data         = #{}               :: #{field() => term()}
    , changes      = #{}               :: #{field() => term()}
    , errors       = []                :: [field_error()]
    , empty_values = [undefined, <<>>] :: nonempty_list()
    }).
-opaque t() :: #changeset{}.

-type changeset()   :: t().
-type field()       :: term().
-type type()        :: atom
                     | binary
                     | bitstring
                     | boolean
                     | float
                     | function
                     | {function, arity()}
                     | integer
                     | list
                     | map
                     | pid
                     | port
                     | record
                     | {record, Name :: atom()}
                     | {record, Name :: atom(), Size :: non_neg_integer()}
                     | reference
                     | tuple
                     .
-type cast_opts()   :: #{empty_values => nonempty_list()}.
-type errmeta()     :: term().
-type errmsg()      :: binary().
-type errmsg_fun()  :: fun((field(), errmeta()) -> errmsg()).
-type error()       :: {errmsg() | errmsg_fun(), errmeta()}.
-type field_error() :: {field(), error()}.
-type pipe()        :: fun((changeset()) -> changeset()).
-type validation()  :: fun((term()) -> [field_error()]).

% Props

-spec get_fields(changeset()) -> [field()].

get_fields(#changeset{fields = Fields}) ->
    Fields.

-spec get_data(changeset()) -> map().

get_data(#changeset{data = Data}) ->
    Data.

-spec get_changes(changeset()) -> map().

get_changes(#changeset{changes = Changes}) ->
    Changes.

-spec get_errors(changeset()) -> [field_error()].

get_errors(#changeset{errors = Errors}) ->
    Errors.

-spec get_errors(field(), changeset()) -> [error()].

get_errors(Field, #changeset{errors = Errors}) ->
    proplists:get_all_values(Field, Errors).

-spec find_data(field(), changeset()) -> {ok, term()} | error.

find_data(Field, #changeset{data = Data}) ->
    maps:find(Field, Data).

-spec get_data(field(), changeset()) -> term() | no_return().

get_data(Field, #changeset{data = Data}) ->
    maps:get(Field, Data).

-spec get_data(field(), changeset(), Default) -> term() | Default.

get_data(Field, #changeset{data = Data}, Default) ->
    maps:get(Field, Data, Default).

-spec find_change(field(), changeset()) -> {ok, term()} | error.

find_change(Field, #changeset{changes = Changes}) ->
    maps:find(Field, Changes).

-spec get_change(field(), changeset()) -> term() | no_return().

get_change(Field, #changeset{changes = Changes}) ->
    maps:get(Field, Changes).

-spec get_change(field(), changeset(), Default) -> term() | Default.

get_change(Field, #changeset{changes = Changes}, Default) ->
    maps:get(Field, Changes, Default).

get_types(#changeset{types = Types}) ->
    Types.

-spec get_type(field(), changeset()) -> type().

get_type(Field, #changeset{types = Types}) ->
    maps:get(Field, Types).

-spec get_empty_values(changeset()) -> list().

get_empty_values(#changeset{empty_values = EmptyValues}) ->
    EmptyValues.

-spec get_required(changeset()) -> [field()].

get_required(#changeset{required = Required}) ->
    Required.

-spec set_required([field()], changeset()) -> changeset().

set_required(Fields, #changeset{} = Changeset) ->
    Changeset#changeset{required = Fields}.

-spec is_valid(changeset()) -> boolean().

is_valid(#changeset{errors = Errors}) ->
    Errors =:= [].

-spec is_invalid(changeset()) -> boolean().

is_invalid(Changeset) ->
    not is_valid(Changeset).

-spec changed(field(), changeset()) -> boolean().

changed(Field, #changeset{changes = Changes}) ->
    is_map_key(Field, Changes).

-spec changed_from(field(), term(), changeset()) -> boolean().

changed_from(Field, ExpectedValue, #changeset{data = From, changes = To})
    when is_map_key(Field, From)
       , is_map_key(Field, To)
       , is_float(ExpectedValue) ->
        maps:get(Field, From) == ExpectedValue;
changed_from(Field, ExpectedValue, #changeset{data = From, changes = To})
    when is_map_key(Field, From)
       , is_map_key(Field, To) ->
        maps:get(Field, From) =:= ExpectedValue;
changed_from(_, _, #changeset{}) ->
    false.

-spec changed_to(field(), term(), changeset()) -> boolean().

changed_to(Field, ExpectedValue, #changeset{changes = To})
    when is_map_key(Field, To)
       , is_float(ExpectedValue) ->
        maps:get(Field, To) == ExpectedValue;
changed_to(Field, ExpectedValue, #changeset{changes = To})
    when is_map_key(Field, To) ->
        maps:get(Field, To) =:= ExpectedValue;
changed_to(_, _, #changeset{}) ->
    false.

% Cast

-spec cast(Payload, Params, Permitted) -> Changeset
    when Payload   :: changeset()
                    | {map(), #{field() := type()}}
       , Params    :: map()
       , Permitted :: [field()]
       , Changeset :: changeset()
       .

cast(Payload, Params, Permitted) ->
    cast(Payload, Params, Permitted, #{}).

-spec cast(Payload, Params, Permitted, Opts) -> Changeset
    when Payload   :: changeset()
                    | {map(), #{field() := type()}}
       , Params    :: map()
       , Permitted :: [field()]
       , Opts      :: cast_opts()
       , Changeset :: changeset()
       .

cast( Changeset = #changeset{ data = Data
                            , types = Types
                            }
    , Params
    , Permitted
    , Opts ) when is_map(Data)
                , is_map(Types)
                , is_map(Params)
                , is_list(Permitted)
                , is_map(Opts) ->
    maps:fold(
        fun(Field, Value, ChangesetAcc) ->
            case should_push_change(Field, Value, Data, Permitted) of
                true ->
                    changeset_type_validator:validate_change(
                        Field, push_change(Field, Value, ChangesetAcc)
                    );
                false ->
                    ChangesetAcc
            end
        end,
        apply_opts(Changeset, Opts),
        Params
    );
cast({Data, Types}, Params, Permitted, Opts) ->
    Changeset = #changeset{ data  = Data
                          , types = Types
                          },
    cast(Changeset, Params, Permitted, Opts).

apply_opts(Changeset, Opts) ->
    maps:fold(fun apply_opt/3, Changeset, Opts).

apply_opt(empty_values, EmptyValues, Changeset) ->
    Changeset#changeset{empty_values = EmptyValues}.

-spec should_push_change(field(), term(), map(), [field()]) -> boolean().

should_push_change(Field, Value, Data, Permitted) ->
    case lists:member(Field, Permitted) of
        true ->
            not is_field_value_equals(Field, Value, Data);
        false ->
            false
    end.

-spec is_field_value_equals(field(), term(), map()) -> boolean().

is_field_value_equals(Field, Value, Data) ->
    case maps:find(Field, Data) of
        {ok, CurrValue} when is_float(Value); is_float(CurrValue) ->
            CurrValue == Value;
        {ok, CurrValue} ->
            CurrValue =:= Value;
        error ->
            false
    end.

% Map

-spec pipe(changeset(), [pipe()]) -> changeset().

pipe(Changeset, Funs) ->
    lists:foldl(fun(F, CSet) -> F(CSet) end, Changeset, Funs).

% Error

-spec error(field(), errmsg(), errmeta()) -> field_error().

error(Field, Msg, Meta) ->
    {Field, {Msg, Meta}}.

-spec push_error(field_error(), changeset()) -> changeset().

push_error( {Field, {Msg, Meta}}
          , #changeset{errors = Errors} = Changeset ) when is_binary(Msg) ->
    Changeset#changeset{errors = [{Field, {Msg, Meta}} | Errors]};
push_error({Field, {MsgFun, Meta}}, Changeset) when is_function(MsgFun, 2) ->
    Msg = MsgFun(Field, Meta),
    push_error({Field, {Msg, Meta}}, Changeset).

-spec push_error(field_error()) -> pipe().

push_error(Error) ->
    fun(Changeset) -> push_error(Error, Changeset) end.

-spec push_errors([field_error()], changeset()) -> changeset().

push_errors(Errors, Changeset) ->
    lists:foldl( fun(Err, CSet) -> push_error(Err, CSet) end
               , Changeset
               , Errors ).

-spec push_errors([field_error()]) -> pipe().

push_errors(Errors) ->
    fun(Changeset) -> push_errors(Errors, Changeset) end.

% Change

-spec push_change(field(), term(), changeset()) -> changeset().

push_change( Field
           , Value
           , #changeset{changes = Changes} = Changeset ) ->
    Changeset#changeset{changes = Changes#{Field => Value}}.

-spec push_change(field(), term()) -> pipe().

push_change(Field, Value) ->
    fun(Changeset) -> push_change(Field, Value, Changeset) end.

-spec push_changes(map(), changeset()) -> changeset().

push_changes(Changes, Changeset) when is_list(Changes) ->
    lists:foldl( fun({Field, Value}, CSet) -> push_change(Field, Value, CSet) end
               , Changeset
               , Changes );
push_changes( Changes
            , #changeset{changes = CurrChanges} = Changeset ) when is_map(Changes) ->
    Changeset#changeset{changes = maps:merge(CurrChanges, Changes)}.

-spec push_changes(map()) -> pipe().

push_changes(Changes) ->
    fun(Changeset) -> push_changes(Changeset, Changes) end.

-spec pop_change(field(), changeset()) -> changeset().

pop_change(Field, Changeset) ->
    pop_changes([Field], Changeset).

-spec pop_change(field()) -> pipe().

pop_change(Field) ->
    fun(Changeset) -> pop_change(Field, Changeset) end.

-spec pop_changes([field()], changeset()) -> changeset().

pop_changes( Fields
           , #changeset{changes = Changes} = Changeset ) when is_list(Fields) ->
    Changeset#changeset{changes = maps:without(Fields, Changes)}.

-spec pop_changes([field()]) -> pipe().

pop_changes(Fields) ->
    fun(Changeset) -> pop_changes(Fields, Changeset) end.

% Validate

-spec validate_change(field(), validation()) -> pipe().

validate_change(Field, Validate) ->
    fun(#changeset{changes = Changes} = Changeset) ->
        validate(Changeset, Field, Validate, Changes)
    end.

-spec validate_change(changeset(), field(), validation()) -> changeset().

validate_change( #changeset{changes = Changes} = Changeset
               , Field
               , Validate ) ->
    validate(Changeset, Field, Validate, Changes).

-spec validate_data(field(), validation()) -> pipe().

validate_data(Field, Validate) ->
    fun(#changeset{data = Data} = Changeset) ->
        validate(Changeset, Field, Validate, Data)
    end.

-spec validate_data(changeset(), field(), validation()) -> changeset().

validate_data( #changeset{data = Data} = Changeset
             , Field
             , Validate ) ->
    validate(Changeset, Field, Validate, Data).

-spec validate(field(), validation(), map()) -> pipe().

validate(Field, Validate, Params) ->
    fun(Changeset) ->
        validate(Changeset, Field, Validate, Params)
    end.

-spec validate(changeset(), field(), validation(), map()) -> changeset().

validate(#changeset{} = Changeset, Field, Validate, Params)
    when is_function(Validate, 1)
       , is_map(Params) ->
    do_validate(Field, Validate, Params, Changeset).

-spec do_validate(field(), validation(), map(), changeset()) -> changeset().

do_validate( Field
           , Validate
           , Params
           , #changeset{errors  = Errors} = Changeset )
    when is_map_key(Field, Params) ->
    case proplists:lookup(Field, Errors) of
        {Field, _} ->
            Changeset;
        none ->
            Value = maps:get(Field, Params),
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

% Helpers

-spec is_field_value_defined(field(), term(), term(), nonempty_list()) -> boolean().

is_field_value_defined(Field, Data, Changes, EmptyValues) ->
    case is_field_value_truthy(Field, Changes, EmptyValues) of
        true ->
            true;
        false ->
            is_field_value_truthy(Field, Data, EmptyValues)
    end.

-spec is_field_value_truthy(field(), map(), list()) -> boolean().

is_field_value_truthy(Field, Payload, EmptyValues) when is_map(Payload) ->
    case maps:find(Field, Payload) of
        {ok, Value} ->
            is_truthy(Value, EmptyValues);
        error ->
            false
    end.

-spec is_truthy(term(), nonempty_list()) -> boolean().

is_truthy(Value, EmptyValues) ->
    not is_falsy(Value, EmptyValues).

-spec is_falsy(term(), nonempty_list()) -> boolean().

is_falsy(Value, EmptyValues) when is_list(EmptyValues) ->
    lists:member(normalize(Value), EmptyValues).

-spec normalize(term()) -> term().

normalize(Value) when is_binary(Value) ->
    string:trim(Value);
normalize(Value) ->
    Value.

% Validators

-spec validate_required([field()]) -> pipe().

validate_required(Fields) ->
    fun(Changeset) ->
        changeset_is_required_validator:validate(Fields, Changeset)
    end.

-spec validate_format(Field, Regexp) -> Pipe
    when Field  :: field()
       , Regexp :: changeset_regexp_validator:regexp()
       , Pipe   :: pipe().

validate_format(Field, Regexp) ->
    validate_format(Field, Regexp, [], []).

-spec validate_format(Field, Regexp, CompileOpts, RunOpts) -> Pipe
    when Field       :: field()
       , Regexp      :: changeset_regexp_validator:regexp()
       , CompileOpts :: [changeset_regexp_validator:compile_option()]
       , RunOpts     :: [changeset_regexp_validator:run_option()]
       , Pipe        :: pipe().

validate_format(Field, Regexp, CompileOpts, RunOpts) ->
    fun(Changeset) ->
        changeset_regexp_validator:validate_change( Field
                                                  , Regexp
                                                  , CompileOpts
                                                  , RunOpts
                                                  , Changeset )
    end.

-spec validate_member(field(), nonempty_list()) -> pipe().

validate_member(Field, List) ->
    fun(Changeset) ->
        changeset_member_validator:validate_change(Field, List, Changeset)
    end.

-spec validate_not_member(field(), nonempty_list()) -> pipe().

validate_not_member(Field, List) ->
    fun(Changeset) ->
        changeset_not_member_validator:validate_change(Field, List, Changeset)
    end.

-ifdef(TEST).

cast_test() ->
    [ { "Should be valid"
      , ?assert(is_valid(
            cast({#{}, #{foo => atom}}, #{foo => bar}, [foo])
        ))
      }
    , { "Should be valid when no params provided"
      , ?assert(is_valid(
            cast({#{}, #{foo => atom}}, #{}, [foo])
        ))
      }
    , { "Should be valid even changes are not permitted"
      , ?assert(is_valid(
            cast({#{}, #{foo => atom}}, #{foo => bar}, [])
        ))
      }
    , { "Should be invalid"
      , ?assertNot(is_valid(
            cast({#{}, #{foo => atom}}, #{foo => <<>>}, [foo])
        ))
      }
    , { "Should return field errors"
      , ?assertEqual(
            [{<<"must be an atom">>, #{validation => is_atom}}]
            , get_errors(foo, cast({#{}, #{foo => atom}}, #{foo => <<>>}, [foo])
        ))
      }
    , { "Should push change"
      , ?assertEqual(#{foo => bar}, get_changes(
            cast({#{}, #{foo => atom}}, #{foo => bar}, [foo])
        ))
      }
    , { "Should not push change"
      , ?assertEqual(#{}, get_changes(
            cast({#{foo => bar}, #{foo => atom}}, #{foo => bar}, [foo])
        ))
      }
    , { "Should preserve changes"
      , ?assertEqual(#{foo => bar, bar => baz}, get_changes(
            cast( cast({#{}, #{foo => atom, bar => atom}}, #{foo => bar}, [foo, bar])
                , #{bar => baz}
                , [foo, bar]
            )
        ))
      }
    ].

-endif.
