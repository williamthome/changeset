-module(changeset_regex_validator).

-export([validate_change/3, validate_change/5]).

-include("changeset.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec validate_change(field(), iodata(), changeset()) -> changeset().

validate_change(Field, Regexp, Changeset) ->
    validate_change(Field, Regexp, [], [], Changeset).

% NOTE: re module does not export types.
-spec validate_change(Field, Regexp, CompileOpts, RunOpts, Changeset) -> Changeset
    when Field       :: field()
       , Regexp      :: iodata()
       , CompileOpts :: list() % [re:compile_option()]
       , RunOpts     :: list() % [re:?]
       , Changeset   :: changeset().

validate_change(Field, Regexp, CompileOpts, RunOpts0, Changeset) ->
    case re:compile(Regexp, CompileOpts) of
        {ok, MP} ->
            RunOpts1 = lists:delete(capture, RunOpts0),
            RunOpts = [{capture, none} | RunOpts1],
            changeset_validator:validate_change(Changeset, Field, fun
                (Subject) ->
                    case re:run(Subject, MP, RunOpts) of
                        match ->
                            [];
                        nomatch ->
                            [ changeset:error( Field
                                             , <<"has invalid format">>
                                             , #{ validation => regex
                                                , regex => Regexp } ) ]
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
            validate_change(foo, "foo", #changeset{changes = #{foo => "foo"}})
        ))
      }
    , { "Should be invalid when no match"
      , ?assertNot(changeset:is_valid(
            validate_change(foo, "foo", #changeset{changes = #{foo => "bar"}})
        ))
      }
    ].

-endif.
