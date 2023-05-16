% NOTE: Fields cannot be a string(). Some functions will
%       crash when traversing the fields list.
-type field()  :: term().
-type meta()   :: term().
-type msg()    :: binary().
-type msgfun() :: fun((field(), meta()) -> binary()).
% TODO: Improve types and find a way to check using dialyzer
-type type()   :: string
                | number
                | map
                | term()
                .
-type error()  :: {field(), {msg(), meta()}}.

-record(changeset,
    { fields       = []         :: [field()]
    , types        = #{}        :: #{field() => [type()]}
    , data         = #{}        :: #{field() => term()}
    , changes      = #{}        :: #{field() => term()}
    , errors       = []         :: [error()]
    , is_valid     = true       :: boolean()
    , default      = no_default :: no_default | fun(() -> term())
    , empty_values = [undefined, <<>>] :: [term()]
    }).
