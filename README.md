# Simple Type Checker
[![Haskell CI](https://github.com/SmnTin/simple-type-checker/actions/workflows/haskell.yml/badge.svg)](https://github.com/SmnTin/simple-type-checker/actions/workflows/haskell.yml)

A type checker for Church-style Symply Typed Lambda Calculus. It takes a typing relation and checks if it is correct.

## Quick start

Firstly, obtain a local copy of the code and move to the project root:
```console
$ git clone https://github.com/SmnTin/simple-type-checker
$ cd simple-type-checker
```

From the project root, where the `package.yaml` is located, run the following to build the project:
```console
$ stack update
$ stack build
```

Then run the type checker while providing it with a typing relation to check. For example:
```console
$ stack run -- "|- \x : A. x  :  A -> A"
Everything is okay :)
```

To run tests type:
```
$ stack test
```

## Syntax

### Type and expression variables

Variables can have names that are allowed in Haskell, i.e. latin letters of both cases, digits, underscores can be used as well as trailing apostrophes, but the name should start with a letter.

Examples of valid names:
`f`, `var`, `LOL228`, `CamelCase`, `xi'''` and `snake_case`

Examples of invalid names:
`strange-case`, `'y`, `8`, `_g`

### Types

A type can be either a type variable or an arrow.

More precisely:

1. If `x` is a type variable then it is a type.
2. If `A` and `B` are types then `A -> B` is a type.

Examples are `x`, `x -> y`, `(x -> y) -> z`, `Nat -> Bool`.

Note that the arrow is right associative:
```
x -> y -> z  =  x -> (y -> z)
```

### Expressions

An expression can be either a variable, an application or a lambda abstraction. Note that Church-style typing is used meaning that the captured variable must be explicitly given a type.

More precisely:

1. If `x` is a variable then `x` is an expression.
2. If `M` and `N` are expressions then the application `M N` is an expression.
3. If `M` is an expression, `x` is a variable and `T` is a type then the lambda abstaction `\x : T. M` is an expression. Variable `x` is said to be abstracted or captured by the lambda.

Note that the application is left associative, e.g.:
```
M N K  =  (M N) K
```

Also note that the lambda spreads right as far as possible, e.g:
```
\x : a. x y  =  \x : a. (x y)
```

Last remark. Even though this is not required, it is still recommended for readability purposes to wrap the type of the captured variable in parentheses if it is an arrow type:
```
\x : (A -> B). x
```

### Environments

An environment is a list of type definitions for expression variables.

If `x` is a variable and `T` is a type then the type definition is written as `x : T`.

The type definitions are listed with a comma:
```
x : A, y : B, z : C
```

Example with more complex types and names:
```
isZero : Nat -> Bool, null : List -> Bool
```

Note that the environment can be empty.

### Typing relations

A typing relation is written as:
```
env |- expr : type
```
where `env` is an environment, `expr` is an expression and `type` is a type.

It is read as "Expression `expr` has type `type` in the environment `env`".

Examples:
```
            |- \x : A. \y : B. x     :  A -> B -> A
     y : A  |- \x : (A -> B). x y    :  A -> B -> A
z : C -> D  |- (\x : (C -> D). x) y  :  C -> D
```
