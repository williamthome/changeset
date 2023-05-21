%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Changeset header.
%%% @end
%%%-----------------------------------------------------------------------------
-record(changeset,
    { fields       = []         :: [field()]
    , types        = #{}        :: #{field() := type()}
    , required     = []         :: [field()]
    , data         = #{}        :: #{field() => term()}
    , changes      = #{}        :: #{field() => term()}
    , errors       = []         :: [error()]
    , is_valid     = true       :: boolean()
    , default      = no_default :: no_default | fun(() -> term())
    , empty_values = [undefined, <<>>] :: [term()]
    }).

-type changeset()  :: #changeset{}.
-type field()      :: term().
-type meta()       :: term().
-type msg()        :: binary().
-type msgfun()     :: fun((field(), meta()) -> binary()).
-type type()       :: atom
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
-type error()      :: {field(), {msg() | msgfun(), meta()}}.
-type pipe()       :: fun((changeset()) -> changeset()).
-type validation() :: fun((term()) -> [error()]).
