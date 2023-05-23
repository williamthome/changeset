# changeset

An OTP library to validate data based on Ecto changeset library (Elixir).

## Disclaimer

This is a WIP library. The code may change at any time without notice.

## Example

Take this simple module:

```erlang
-module(movie).

-export([changeset/1, changeset/2]).

-define(TYPES,     #{name => binary}).
-define(PERMITTED, maps:keys(?TYPES)).
-define(REQUIRED,  maps:keys(?TYPES)).

changeset(Params) ->
    changeset(#{}, Params).

changeset(Data, Params) ->
    Changeset = changeset:cast({Data, ?TYPES}, Params, ?PERMITTED),
    changeset:pipe(Changeset, [
        changeset:validate_required(?REQUIRED)
        % More validators here, e.g.:
        % changeset:validate_change(name, fun(_Name) -> [] end)
    ]).
```

Now running

```shell
rebar3 shell
```

we can type the following:

```erlang
% The name is missing, the changeset will be invalid
1> movie:changeset(#{}).
{changeset,[],
           #{name => binary},
           [name],
           #{},#{},
           [{name,{<<"is required">>,#{validation => is_required}}}],
           false,no_default,
           [undefined,<<>>]}

% The name is not a binary, the changeset will be invalid
2> movie:changeset(#{name => foo}).
{changeset,[],
           #{name => binary},
           [name],
           #{},
           #{name => foo},
           [{name,{<<"must be a binary">>,#{validation => is_binary}}}],
           false,no_default,
           [undefined,<<>>]}

% The name is present and it's a binary, then the changeset will be valid
3> movie:changeset(#{name => <<"Erlang: The Movie">>}).
{changeset,[],
           #{name => binary},
           [name],
           #{},
           #{name => <<"Erlang: The Movie">>},
           [],true,no_default,
           [undefined,<<>>]}
```

## Struct

Currently, this is the changeset record

```erlang
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
```

and this are the available field types:

```erlang
-type type() :: atom
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
```

The types are auto validated by the cast function.
