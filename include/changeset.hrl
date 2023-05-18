% NOTE: Fields cannot be a string(). Some functions will
%       crash when traversing the fields list.
-type field()  :: term().
-type meta()   :: term().
-type msg()    :: binary().
-type msgfun() :: fun((field(), meta()) -> binary()).
-type type()   :: atom
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
                | {record, RecordTag :: atom()}
                | {record, RecordTag :: atom(), Size :: non_neg_integer()}
                | reference
                | tuple
                | term()
                .
-type error()  :: {field(), {msg(), meta()}}.

-record(changeset,
    { fields       = []         :: [field()]
    , types        = #{}        :: #{field() => type()}
    , data         = #{}        :: #{field() => term()}
    , changes      = #{}        :: #{field() => term()}
    , errors       = []         :: [error()]
    , is_valid     = true       :: boolean()
    , default      = no_default :: no_default | fun(() -> term())
    , empty_values = [undefined, <<>>] :: [term()]
    }).
