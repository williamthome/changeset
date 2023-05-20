-module(changeset_type_validator).

-include("changeset.hrl").

-callback validate_change(field(), #changeset{}) -> [error()].
