% NOTE: Fields cannot be a string(). Some functions will
%       crash when traversing the fields list.
-type field() :: term().
-type type()  :: string
               | number
               | map
               | term().
-type error() :: {field(), {string(), [tuple()]}} | term().

-record(changeset,
    { fields       = []         :: [field()]
    , types        = #{}        :: #{field() => type() | [type()]}
    , data         = #{}        :: #{field() => term()}
    , changes      = #{}        :: #{field() => term()}
    , errors       = []         :: [error()]
    , is_valid     = true       :: boolean()
    , default      = no_default :: no_default | fun(() -> term())
    , empty_values = [undefined, <<>>] :: [term()]
    }).
