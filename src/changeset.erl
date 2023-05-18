-module(changeset).

-export([is_valid/1]).
-export([fold/2]).
-export([error/3]).
-export([push_error/1, push_error/2]).
-export([push_errors/1, push_errors/2]).
-export([push_change/2, push_change/3]).
-export([push_changes/1, push_changes/2]).
-export([pop_change/1, pop_change/2]).
-export([pop_changes/1, pop_changes/2]).

-include("changeset.hrl").

% Props

is_valid(#changeset{is_valid = IsValid}) ->
    IsValid.

% Map

fold(Funs, Changeset) ->
    lists:foldl(fun(F, CSet) -> F(CSet) end, Changeset, Funs).

% Error

error(Field, Msg, Meta) ->
    {Field, {Msg, Meta}}.

push_error( {Field, {Msg, Meta}}
          , #changeset{errors = Errors} = Changeset) when is_binary(Msg) ->
    Changeset#changeset{ errors = [{Field, {Msg, Meta}} | Errors]
                    , is_valid = false };
push_error({Field, {MsgFun, Meta}}, Changeset) when is_function(MsgFun, 2) ->
    Msg = MsgFun(Field, Meta),
    push_error({Field, {Msg, Meta}}, Changeset).

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
