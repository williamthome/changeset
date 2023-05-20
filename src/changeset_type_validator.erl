-module(changeset_type_validator).

-export([validate_change/2]).

-include("changeset.hrl").

-callback validate_change(field(), #changeset{}) -> [error()].

validate_change(Field, #changeset{types = Types} = Changeset) ->
    FieldType = maps:get(Field, Types),
    do_validate_change(FieldType, Field, Changeset).

do_validate_change(atom, Field, Changeset) ->
    changeset_type_validator_is_atom:validate_change(Field, Changeset);
do_validate_change(binary, Field, Changeset) ->
    changeset_type_validator_is_binary:validate_change(Field, Changeset);
do_validate_change(bitstring, Field, Changeset) ->
    changeset_type_validator_is_bitstring:validate_change(Field, Changeset);
do_validate_change(boolean, Field, Changeset) ->
    changeset_type_validator_is_boolean:validate_change(Field, Changeset);
do_validate_change(float, Field, Changeset) ->
    changeset_type_validator_is_float:validate_change(Field, Changeset);
do_validate_change(function, Field, Changeset) ->
    changeset_type_validator_is_function:validate_change(Field, Changeset);
do_validate_change({function, Arity}, Field, Changeset) ->
    changeset_type_validator_is_function:validate_change({Field, Arity}, Changeset);
do_validate_change(integer, Field, Changeset) ->
    changeset_type_validator_is_integer:validate_change(Field, Changeset);
do_validate_change(list, Field, Changeset) ->
    changeset_type_validator_is_list:validate_change(Field, Changeset);
do_validate_change(map, Field, Changeset) ->
    changeset_type_validator_is_map:validate_change(Field, Changeset);
do_validate_change(pid, Field, Changeset) ->
    changeset_type_validator_is_pid:validate_change(Field, Changeset);
do_validate_change(port, Field, Changeset) ->
    changeset_type_validator_is_port:validate_change(Field, Changeset);
do_validate_change(record, Field, Changeset) ->
    changeset_type_validator_is_record:validate_change(Field, Changeset);
do_validate_change({record, Name}, Field, Changeset) ->
    changeset_type_validator_is_record:validate_change({Field, Name}, Changeset);
do_validate_change({record, Name, Size}, Field, Changeset) ->
    changeset_type_validator_is_record:validate_change({Field, Name, Size}, Changeset);
do_validate_change(reference, Field, Changeset) ->
    changeset_type_validator_is_reference:validate_change(Field, Changeset);
do_validate_change(tuple, Field, Changeset) ->
    changeset_type_validator_is_tuple:validate_change(Field, Changeset).
