%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Not a list member validator module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(changeset_not_member_validator).

-export([validate_change/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type changeset() :: changeset:t().
-type field()     :: changeset:field().

-spec validate_change(field(), nonempty_list(), changeset()) -> changeset().

validate_change(Field, List, Changeset) ->
    changeset:validate_change(Changeset, Field, fun
        (Elem) ->
            case lists:member(Elem, List) of
                false ->
                    [];
                true ->
                    [ changeset:error( Field
                                    , <<"is invalid">>
                                    , #{ validation => not_member
                                       , list => List
                                       , element => Elem } ) ]
            end
    end).

-ifdef(TEST).

validate_change_test() ->
    [ { "Should be valid"
      , ?assert(changeset:is_valid(
            validate_change(foo, [foo],
                changeset:cast({#{}, #{foo => atom}}, #{foo => bar}, [foo])
            )
        ))
      }
    , { "Should be invalid when element is a list member"
      , ?assertNot(changeset:is_valid(
          validate_change(foo, [bar],
              changeset:cast({#{}, #{foo => atom}}, #{foo => bar}, [foo])
          )
        ))
      }
    ].

-endif.
