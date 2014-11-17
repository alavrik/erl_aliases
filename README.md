Overview
--------

`erl_aliases` is an Erlang parse transformation library that provides a simple
and straightforward interface for defining (shorter) aliases for (longer) record
and module names. Once defined, aliases can be used instead of the original
names.


Rationale
---------

Global Erlang record and module names tend to be relatively long. Long names
are usually more descriptive and help to keep names unique across different
Erlang applications. However, long names can sometimes be cumbersome inside
application code.

For instance, it can be fairly annoying to type a long record name every
time just to access a field. But more importantly, long record and module
names make code less readable.


Usage
-----

```erlang
    % include a header that specifies Erl_aliases parse transformation function
    -include_lib("erl_aliases.hrl").
    ...

    -record(long_record_name, {...}). % an Erlang record definition
    ...

    % define alias 'r' for the previously defined record 'long_record_name':
    -record_alias({r, long_record_name}).
    ...

    % define alias 'm' for module 'long_module_name':
    -module_alias({m, long_module_name}).
    ...
```

After that, `r` can be used instead of `long_record_name` in operations with
records, and `m` instead of `long_module_name` in module context.


Details
-------

When an Erlang module that uses `erl_aliases` is compiled, its input AST is
automatically gets transformed as follows.

- All record and module aliases appearing in valid contexts are renamed back to
  their original names.
- Alias definitions are removed from the input AST.

There are several restrictions.

- Aliases can only be defined for original record and module names. An alias can
  not be defined for another alias.
- Record aliases are not allowed to mask previously defined record names and
  vice versa.

If any of the above restrictions is violated, compilation will fail and an error
message will be printed.


Limitations
-----------

Parse transformations do not work in Erlang interactive shell.

Defined record aliases can not be used inside `-spec` and `-record` forms.

Using aliases with `is_record` and `record_info` built-in functions is not
supported but may be added later.


Authors
-------

`erl_aliases` is written by Anton Lavrik <alavrik@piqi.org>


License
-------

`erl_aliases` is distributed under the terms of a MIT license. See the LICENSE
file for details.

