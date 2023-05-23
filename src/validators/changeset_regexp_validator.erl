-module(changeset_regexp_validator).

-export([validate_change/5]).

-export_type([regexp/0, compile_option/0, run_option/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type changeset()      :: changeset:t().
-type field()          :: changeset:field().
-type regexp()         :: iodata().
-type compile_option() :: term(). % NOTE: re module does not export re:compile_option().
-type run_option()     :: term(). % NOTE: re module has no type to this option.

-spec validate_change(Field, Regexp, CompileOpts, RunOpts, Changeset) -> Changeset
    when Field       :: field()
       , Regexp      :: regexp()
       , CompileOpts :: [compile_option()]
       , RunOpts     :: [run_option()]
       , Changeset   :: changeset().

validate_change(Field, Regexp, CompileOpts, RunOpts0, Changeset) ->
    case re:compile(Regexp, CompileOpts) of
        {ok, MP} ->
            RunOpts1 = lists:delete(capture, RunOpts0),
            RunOpts = [{capture, none} | RunOpts1],
            changeset:validate_change(Changeset, Field, fun
                (Subject) ->
                    case re:run(Subject, MP, RunOpts) of
                        match ->
                            [];
                        nomatch ->
                            [ changeset:error( Field
                                             , <<"has invalid format">>
                                             , #{ validation => regexp
                                                , regexp => Regexp } ) ]
                    end
            end);
        {error, ErrSpec} ->
            error( {invalid_regex, ErrSpec}
                 , [Field, Regexp, CompileOpts, RunOpts0, Changeset] )
    end.

-ifdef(TEST).

validate_change_test() ->
    [ { "Should be valid"
      , ?assert(changeset:is_valid(
            validate_change(foo, <<"foo">>, [], [],
                changeset:cast({#{}, #{foo => binary}}, #{foo => <<"foo">>}, [foo])
            )
        ))
      }
    , { "Should be invalid when no match"
      , ?assertNot(changeset:is_valid(
            validate_change(foo, <<"foo">>, [], [],
                changeset:cast({#{}, #{foo => binary}}, #{foo => <<"bar">>}, [foo])
            )
        ))
      }
    ].

-endif.
