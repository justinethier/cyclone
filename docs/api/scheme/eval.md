---
layout: main
title: API
---

# Eval Library

The `(scheme eval)` library exports procedures for evaluating Scheme data as programs.

For more information see the [R<sup>7</sup>RS Scheme Specification](../../r7rs.pdf).

- [`eval`](#eval)
- [`create-environment`](#create-environment)
- [`setup-environment`](#setup-environment)
- [`expand`](#expand)

# eval

    (eval expr . environment)

Evaluate `expr` in the specified environment and return the resulting value(s).

A non-standard extension is provided to supply a default global environment if the `environment` argument is not specified.

# create-environment

    (create-environment vars values)

A non-standard function to create a new environment on top of the default one.

`vars` is a list of identifiers in the new environment, and `values` is a list of each value assigned to each identifier.

# setup-environment


    (setup-environment)

A non-standard function to initialize a new global environment.

# expand

    (expand expr [[environment] [rename-environment]])

Perform macro expansion on `expr` and return the resulting expression.

`environment` may be optionally passed as the current environment. 

`rename-environment` is an optional argument of an environment containing variables renamed directly by macros. This would generally be an empty environment when using this function for macro debugging.

