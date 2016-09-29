# Process-Context Library

The `(scheme process-context)` library exports procedures for accessing with the program's calling context.

- [`command-line`](#command-line)
- [`emergency-exit`](#emergency-exit)
- [`get-environment-variable`](#get-environment-variable)
- [`get-environment-variables`](#get-environment-variables)

#command-line

    (command-line)

Returns the command line passed to the program as a list of strings.

#emergency-exit

    (emergency-exit)

Terminates the program immediately. This is an alias of `exit`. 

#get-environment-variable

    (get-environment-variable name)

Return the value of the given environment variable.

#get-environment-variables

    (get-environment-variables)

Return a list of all the environment variables and their values.

