# ApolloCAS

A computer algebra system (CAS) built into a custom scripting language. Please note that this project is still in development. The project is on a hiatus right now, as I am writing my college apps, but it will be back in development by early November!

## Build and Run

As of now, there are no releases, so you will need to build from source. This project is written in Rust, so you will need to install Cargo to build it (see https://doc.rust-lang.org/cargo/getting-started/installation.html).
To run the project in REPL mode, run `cargo run`. To run a file, just supply the file's path as the first argument: `cargo run--file/path/here`. If you are unfamiliar with cargo, you can use the --release flag to build in release mode.

## Part 1: ApolloCAS the Programming Language

The programming language portion of ApolloCAS is currently the much more mature section (the other section being the CAS). It includes much of the basic functionality you would expect in a scripting language, including...

### Operators
#### Arithmetic

ApolloCAS includes all the basic operations you would expect. Example: `(1 - 2 + 1) * 3^4`. Please note that the '^' operator performs exponentiation, not a bitwise or.

#### Logical
Includes `and`, `or`, `<=`, `>=`, `=` (NOT `==`), `!=`, `!`, `<`, and `>`.
These work exactly like you would expect, except for the numerical comparison operators, which chain. Example: `3 >= 2 != 1` evaluates to true.
#### Concatenation
You can concatenate strings, arrays, or any combination of the two using the `++` operator, NOT the addition operator.
Example:
```
let l = [1, 2, 3]
let s = "Hello"
println(l ++ l)           # prints [1,2,3,1,2,3]
println([] ++ s)    # prints ['H', 'e', 'l', 'l', 'o']
println("" ++ l)          # prints "123"
println(s ++ ", world!")   # prints "Hello, world!")
```

### Variables
#### Initialization
ApolloCAS variables are defined using the syntax `let a = 3`. Optionally, include a type annotation `let a: Int = 3`. If unspecified, types are assigned at runtime (Auto) rather than being left as Any.
Variable shadowing is allowed if and only if the shadowing occurs in a different scope.
Example:
```
let a = 3
{
  let a = 5
  println(a)    # Prints 5
}
println(a)      # Prints 3
```
Note that newlines rather than semicolons separate statements.
#### Setting
You can set a variable to a new value by using the set keyword: `set a = 4`. Just saying `a = 4` is interpreted as a boolean expression returning whether or not 'a' equals 4.
#### Variable Types
The Variable types include:
- Any: a flexible type that can be assigned to anything
- Int: an integer
- Float: a double-precision floating-point number
- Arr: an array of values of any type `[1, 2, 3]`
- Iter: an iterator
- BuiltIn: built-in functions like println
- Bool: a boolean `true` or `false`
- Unit: essentially void or null, but far less scary `{}` (may be changed to None in later versions)
- Str: a string `"example"`
- Char: a single ASCII character `'a'`
- Auto: default type that morphs into the type of the value to which it is assigned
- Sym: a symbolic type which includes the subtypes Z (a big integer), Symbol (a symbol), and P[_] (a polynomial in _ where _ is a placeholder for a variable)
- (_, _) -> _: the type of a function, where the underscores are stand-ins for any of the above types

#### Control flow

The three major types of control flow are while, for, and if. If you are familiar with programming, these work exactly as you would expect them to. Example:
```
if true {    # No parenthesis necessary
  println("This always prints")
} else {
  println("This never prints")
}

# Prints even numbers from 0-10
for i in range(0, 11, 2) {
  println(i)
}

while 1 = 1 {        # Note that this is a boolean expression, not a set statement
  println("This won't stop, will it...")
}
```
For loops are range-based rather than C-style. They take an argument, and that argument must be able to be converted into an iterator, such as an array.

### User-Defined Functions

One of the unique features of ApolloCAS is that all functions are anonymous, and functions are treated like values. This is the syntax for creating a function.
```
let add = fn(a: Int, b: Int) -> Int {
  return a + b
}
```
Which is the same as writing `let add = fn(a: Int, b: Int) -> Int: a + b`. Both the type annotations for the parameters and the return type are optional but highly recommended. The default parameter type is Any, and the default return type is Unit.
The Reason for the slightly odd design choice is to simplify the syntax of the language by reducing the need for a separate syntax for anonymous functions while highlighting the first-class nature of ApolloCAS functions
(meaning functions can be used as inputs and outputs of other functions).
Speaking of first-class functions, here is an example:
```
let map = fn(arr: Arr, f: (Any -> Any)) -> Arr {
    let mapped = []
    for e in arr {
        push(mapped, f(e))
    }
    return mapped
}

println(map([1,2,3], fn(x: Any) -> Any: x * x))
```
Here, we use first-class functions to implement the map function, then use said map function to print the first 3 square numbers.

### Built-in Functions

There are various built-in functions that you don't need to implement yourself:
- print: prints expression with no newline
- println: prints expression with a newline
- log: the log function takes one required argument and optionally the base of the log
- ln: the natural log
- sqrt: the square root
- sin, cos, and tan
- exit: breaks out of the program, returning 0 by default or the provided exit code
- clock: returns time in milliseconds
- sleep: pauses execution for the provided number of milliseconds
- push: pushes a value to the end of an array
- pop: pops the top value from the end of the array, returning it
- range: takes either 1 argument, going from 0 to n exclusively, or 3 arguments, a start, step, and increment
- len: returns the length of an array
- copy: copies a value, allocating entirely new memory rather than just copying the reference
- type: returns the stringified type of a value
- iter: returns a value converted to an iterator
- next: returns the next item in an iterator or the unit
- gcd: returns the gcd of an Int, Float, Z, or P[_]
- poly: takes an expression and a variable and returns a polynomial in that variable
- treeify: takes a polynomial into a symbolic expression

### Memory
Memory is managed via a garbage collector. This means there is no need to call free or malloc. We're saved!
Types like Arr and Sym are stored by reference and live in the "heap" and are managed by the garbage collector.
Primitives like Bool and Int are stored locally.

## Part 2: ApolloCAS the Computer Algebra System

This is the much less mature part of ApolloCAS, and much work is still to be done, but here are the features thus far.

### Initalization

There are two ways of initializing symbolic expressions, through type annotations or a symbolic annotation.
```
# Type annotation
let a: Sym = x = 1
# Symbolic annotation
let b = $x + 1
```
In the first case, ApolloCAS knows to treat the first expression like a symbolic expression because it has the type Sym (this also works with types the symbolic subtypes).
By default, when we initialize a symbolic expression, it is fully expanded and initialized (though I am not happy with this behavior, so I will revisit it in the future).
For example, `$(x+1)^2` would evaluate to the same thing as `$x^2 + 2 * x + 1`. Note that here, 'x' is not a program variable; it is a symbol.

### Types
Types of symbolic expressions form a tree structure where Sym is the root of the tree and Z/P[\_]/Symbol are the leaves. Meaning values of type P[_], Z, and Symbol can be assigned to a variable with type Sym, but this is not true the other way around.
Here is a description of all the types (this list is short for now, but will grow in time):
- Sym: any symbolic type
- Z: an (almost) arbitrarily big integer `$10^200  # can be very big`
- P[_]: a univariat polynomial in _ where _ is a symbol `poly(3*x^2+y^2*x + 1, x)    # here y is treated like a constant`
- Symbol: a symbol represented by an identifier `$doesnt_have_to_be_a_single_letter`


### Polynomials
Currently, the only type of polynomial that is supported is univariate polynomials.
We can declare a polynomial by using the poly function, and we can add, subtract, and multiply polynomials together. For example, `poly(x+1,x) * (poly(3 * x + 3, x) + poly(x,x))`.
If we try to multiply two polynomials in different variables, we simply convert them both to tree-based expressions and multiply them that way.
If we take a binomial to a power, we apply the binomial theorem to efficiently expand it. Try `poly(a+b,a)^20`.

## TODO
Before I can call this project complete, here is the list of things I have yet to implement:
- GCDs for polynomials
- rational numbers and functions
- multivariate polynomials
- equation solving of some sort
- log and trig functions
- derivatives
- structs
- modules
- the pipe operator
- a LOT of bug fixes

So yeah... there's a long way to go!
