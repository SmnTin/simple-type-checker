# Simple Type Checker
[![Haskell CI](https://github.com/SmnTin/simple-type-checker/actions/workflows/haskell.yml/badge.svg)](https://github.com/SmnTin/simple-type-checker/actions/workflows/haskell.yml)

A type checker for Church-style Polymorphic Lambda Calculus (System F). It takes a typing relation and checks if it is correct.

## Quick start

Firstly, obtain a local copy of the code and move to the project root:
```console
$ git clone --branch system-f https://github.com/SmnTin/simple-type-checker
$ cd simple-type-checker
```

From the project root, where the `package.yaml` is located, run the following to build the project:
```console
$ stack update
$ stack build
```

Then run the type checker while providing it with a typing relation to check. For example:
```console
$ stack run -- "|-  \x : A. x  :  A -> A"
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

A type can be either a type variable, an arrow or a forall quantifier.

More precisely:

1. If `a` is a type variable then it is a type.
2. If `A` and `B` are types then `A -> B` is a type.
3. If `a` is a type variable and `A` is a type then `@a. A` is a type. `@` is used to denote "forall" quantifier.

Examples are `x`, `x -> y`, `(x -> y) -> z`, `Nat -> Bool`.

Note that the arrow is right associative:
```
x -> y -> z  =  x -> (y -> z)
```

Also note that the quantifier spreads to the right as far as possible, e.g.:
```
@a. a -> b  =  @a. (a -> b)
```

### Expressions

An expression can be either a variable, an application, type application, a lambda abstraction or a type lambda abstaction. Note that Church-style typing is used meaning that the captured variable must be explicitly given a type.

More precisely:

1. If `x` is a variable then `x` is an expression.
2. If `M` and `N` are expressions then the application `M N` is an expression.
3. If `M` is an expression and `T` is a type then the type application `M [T]` is an expression.
3. If `M` is an expression, `x` is a variable and `T` is a type then the lambda abstaction `\x : T. M` is an expression. `\` is used to denote small lambda. Variable `x` is said to be abstracted, binded or captured by the lambda.
4. If `M` is an expression, `a` is a type variable then the type abstaction `#a. M` is an expression. `#` is used to denote big lambda. Type variable `a` is said to be abstracted, binded or captured by the lambda. 

Note that the application as well as type application is left associative and both of them have the same priority, e.g.:
```
M N K       =  (M N) K
M [T] [R]   =  (M [T]) [R]
M [T] N [R] =  ((M [T]) N) [R]
```

Also note that the lambdas spread right as far as possible, e.g:
```
\x : a. x y      =  \x : a. (x y)
#a. \x : a. x y  =  #a. (\x : a. (x y))
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
isZero : Nat -> Bool, null : List -> Bool, id : @a. a -> a
```

Note that the environment can be empty.

### Typing relations

A typing relation is written as:
```
env |- expr : type
```
where `env` is an environment, `expr` is an expression and `type` is a type.

It is read as "Expression `expr` has type `type` in the environment `env`" or "Expression `expr` is of type `type` in the environment `env`".

Examples:
```
            |- \x : A. \y : B. x     :  A -> B -> A
     y : A  |- \x : (A -> B). x y    :  A -> B -> A
z : C -> D  |- (\x : (C -> D). x) y  :  C -> D
```

## Typing rules

### Type substitution

Substitution of a type `T` into a type instead of a type variable `a` is basically just the same type with all the occurrences of `a` replaced with `T`. Denoted as `[a := T] ...`

Substitution into a type more precisely:

1. `[a := T] a      :=  T` 
2. `[a := T] b      :=  b`
3. `[a := T] F -> R := [a := T] F -> [a := T] R`
4. `[a := T] @b. F  := @b. [a := T] F`

### Free variables

Free variables are expression or type variables which are not captured by any lambdas or quantifiers.

Type of a free expression variable should be defined in the environment:
```
..., x : T  |-  x : T
```

### Lambda abstraction

Inferring type of a lambda is just adding an arrow:
```
..., x : T  |-  M : R  =>  ... |-  \x : T. M  : T -> R 
```

Note that variable `x` popped out of the environment because it is not a free variable anymore, it was captured by lambda.

### Type lambda abstraction

Inferring type of a type lambda is just adding a quantifier. Formally:
```
... |- M : T  =>  ... |- #a. M  :  @a. T
```

### Application

Expressions application `F X` can be intuitively understood as calling function `F` with the argument `X`. Therefore, the type of the applied argument must be the same as which the function expects.

Formally:
```
... |- F : T -> R,
... |- X : T
=> ... |- F X : R
```

### Type application

Type application `F [T]` is like usual application but on the type level. `F` must be of quantifier type. Type `T` is substituted instead of quantified variable.

Formally:
```
... |- F : @a. R   =>  ... |- F [T] : [a := T] R
```

## Variable shadowing

Variable shadowing is a situation in which the variable is captured twice.

This type checker allows expression variable shadowing.
For example, the following expression is completely valid:
```
\x : a. \x : b. x
```
These variables are considered implicitly different.

On the other hand, type variable shadowing is disallowed. For example, type inference of the following expressions would result in an error:
```
#a. #a. \x : a. x
\x : (@a. @a. a). x
```

The more complex example of the invalid expression is:
```
(#a. \x : a. x) [a]
```
This is because all the free variables are considered captured by some outer lambdas.

Such rules are introduced to disallow such expressions:
```
(#a. #b. \x : a. \y : b. x) [b]
```

If we do blind substitution, we get:
```
[a := b] (@a. @b. a -> b -> a) =  @b. b -> b
```
This is known as implicit variable capture problem. To address this issue such techniques as auto-renaming or using other type representations such as de Bruijn indices are employed.
