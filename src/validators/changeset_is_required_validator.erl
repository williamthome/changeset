-module(changeset_is_required_validator).

-export([validate/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type changeset() :: changeset:t().
-type field()     :: changeset:field().

-spec validate([field()], changeset()) -> changeset().

validate(Fields, Changeset0) when is_list(Fields) ->
    Changeset   = changeset:set_required(Fields, Changeset0),
    Data        = changeset:get_data(Changeset),
    Changes     = changeset:get_changes(Changeset),
    EmptyValues = changeset:get_empty_values(Changeset),
    do_validate(Fields, Data, Changes, EmptyValues, Changeset).

-spec do_validate([field()], map(), map(), list(), changeset()) -> changeset().

do_validate([Field | T], Data, Changes, EmptyValues, Changeset) ->
    case changeset:is_field_value_defined(Field, Data, Changes, EmptyValues) of
        true ->
            do_validate(T, Data, Changes, EmptyValues, Changeset);
        false ->
            Error = changeset:error( Field
                                   , <<"is required">>
                                   , #{validation => is_required} ),
            changeset:pipe( Changeset
                          , [ changeset:pop_change(Field)
                            , changeset:push_error(Error) ] )
    end;
do_validate([], _, _, _, Changeset) ->
    Changeset.

-ifdef(TEST).

validate_test() ->
    ChangesChangeset = changeset:cast({ #{}, #{foo => atom} }, #{foo => bar}, [foo]),
    ValidChanges = validate([foo], ChangesChangeset),
    InvalidChanges = validate([bar], ChangesChangeset),

    DataChangeset = changeset:cast({ #{foo => bar}, #{foo => atom} }, #{}, [foo]),
    ValidData = validate([foo], DataChangeset),
    InvalidData = validate([bar], DataChangeset),

    Required = validate([foo, bar], changeset:cast({#{}, #{foo => atom, bar => atom}}, #{}, [foo, bar])),

    [ { "Should have valid changes"
      , ?assert(changeset:is_valid(ValidChanges))
      }
    , { "Should have invalid changes"
      , ?assertNot(changeset:is_valid(InvalidChanges))
      }
    , { "Should have valid data"
      , ?assert(changeset:is_valid(ValidData))
      }
    , { "Should have invalid data"
      , ?assertNot(changeset:is_valid(InvalidData))
      }
    , { "Should have required fields"
      , ?assertEqual([foo, bar], changeset:get_required(Required))
      }
    ].

-endif.
