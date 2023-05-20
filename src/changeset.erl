%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Changeset module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(changeset).

-export([is_valid/1, get_changes/1]).
-export([cast/3, cast/4]).
-export([pipe/2]).
-export([error/3]).
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

% Props

is_valid(#changeset{is_valid = IsValid}) ->
    IsValid.

get_changes(#changeset{changes = Changes}) ->
    Changes.

% Cast

cast(Payload, Params, Permitted) ->
    cast(Payload, Params, Permitted, #{}).

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
    Changes =
        maps:filter(
            fun(Field, Value) ->
                case lists:member(Field, Permitted) of
                    true ->
                        case maps:find(Field, Data) of
                            {ok, CurrValue} when is_float(Value)
                                               ; is_float(CurrValue) ->
                                CurrValue /= Value;
                            {ok, CurrValue} ->
                                CurrValue =/= Value;
                            error ->
                                true
                        end;
                    false ->
                        false
                end
            end,
            Params
        ),
    lists:foldl(
        fun(Field, ChangesetAcc) ->
            FieldType = maps:get(Field, Types),
            changeset_validator:validate_change_by_field_type(FieldType, Field, ChangesetAcc)
        end,
        % TODO: Check if changes should be merged instead of overridden
        Changeset#changeset{changes = Changes},
        maps:keys(Changes)
    );
cast({Data, Types}, Changes, Permitted, Opts) ->
    Changeset = #changeset{ data  = Data
                          , types = Types
                          },
    cast(Changeset, Changes, Permitted, Opts).

% Map

pipe(Changeset, Funs) ->
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

% Test

-ifdef(TEST).

cast_test() ->
    [ { "Should be valid"
      , ?assert(is_valid(cast( { #{}, #{foo => binary} }
                             , #{foo => <<>>}
                             , [foo]
                             )
                        )
               )
      }
    , { "Should be invalid"
      , ?assertNot(is_valid(cast( { #{}, #{foo => binary} }
                                , #{foo => bar}
                                , [foo]
                                )
                           )
                  )
      }
    , { "Should push change"
      , ?assertEqual( #{foo => <<>>}
                    , get_changes(cast( { #{}, #{foo => binary} }
                                      , #{foo => <<>>}
                                      , [foo]
                                      )
                                 )
                    )
      }
    , { "Should not push change"
      , ?assertEqual( #{}
                    , get_changes(cast( { #{foo => <<>>}, #{foo => binary} }
                                      , #{foo => <<>>}
                                      , [foo]
                                      )
                                 )
                    )
      }
    ].

-endif.
