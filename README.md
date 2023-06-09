# changeset

An OTP library to validate data based on Ecto changeset library (Elixir).

## Disclaimer

This is a WIP library. The code may change at any time without notice.

## Example

Take this simple module:

```erlang
-module(movie).

-export([changeset/1, changeset/2]).

-define(TYPES,     #{name => binary, starring => binary}).
-define(PERMITTED, maps:keys(?TYPES)).
-define(REQUIRED,  [name]).

changeset(Params) ->
    changeset(#{}, Params).

changeset(Data, Params) ->
    Changeset = changeset:cast({Data, ?TYPES}, Params, ?PERMITTED),
    changeset:pipe(Changeset, [
        changeset:validate_required(?REQUIRED)
        % More validators here, e.g.:
        % changeset:validate_change(name, fun(_Name) -> [] end)
        % changeset:validate_format(name, "^[A-Z]")
        % changeset:validate_member(starring, [<<"Mike">>, <<"Joe">>, <<"Robert">>])
        % changeset:validate_not_member(starring, [<<"Me">])
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
           #{name => binary,starring => binary},
           [name],
           #{},#{},
           [{name,{<<"is required">>,#{validation => is_required}}}],
           [undefined,<<>>]}

% The name is not a binary, the changeset will be invalid
2> movie:changeset(#{name => foo}).
{changeset,[],
           #{name => binary,starring => binary},
           [name],
           #{},
           #{name => foo},
           [{name,{<<"must be a binary">>,#{validation => is_binary}}}],
           [undefined,<<>>]}

% The name is present and it's a binary, then the changeset will be valid
3> movie:changeset(#{name => <<"Erlang: The Movie">>}).
{changeset,[],
           #{name => binary,starring => binary},
           [name],
           #{},
           #{name => <<"Erlang: The Movie">>},
           [],
           [undefined,<<>>]}

% Get the valid changes
4> changeset:get_changes(v(3)).
#{name => <<"Erlang: The Movie">>}
```

## Struct

Currently, this is the changeset record

```erlang
-record(changeset,
    { fields       = []                :: [field()]
    , types        = #{}               :: #{field() := type()}
    , required     = []                :: [field()]
    , data         = #{}               :: #{field() => term()}
    , changes      = #{}               :: #{field() => term()}
    , errors       = []                :: [error()]
    , empty_values = [undefined, <<>>] :: nonempty_list()
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
