Exercises
=========

### Introduction

* Get started following the guide here: https://www.rust-lang.org/
* Follow tutorial here: https://doc.rust-lang.org/cargo/

> [!IMPORTANT]
>
> PLEASE FOLLOW THE FOLLOWING RULES CAREFULLY. They're needed to grade your
> submissions, and any deviation will be penalized

* Create one project for the whole TP with `cargo init tp2`
* Create one file per exercize, named `ex<N>.rs` where N is the number of the
  exercize (so `ex1.rs` for exercize 1)
* In the main, include `mod ex<N>` directives so that the ex files are
  compiled. Your main should look something like this:

```rust
mod ex1;
mod ex1b;
mod ex2;
mod ex3;
mod ex4;
mod ex6;
mod ex7;
mod ex7b;
mod ex8;

fn main() {
}
```

* Respect the naming of functions that is required in the following exercizes.

### Exercise 1

Write the body of the following `invert` function:

```rust
fn invert(s: &String) -> String {
}
```

`invert` shall return the inverted string

Write the associated tests:

```rust
#[cfg(test)]
mod tests {
    #[test]
    fn ...() {
    }
}
```

### Exercise 1b

Write the same function but inverting the string in place

```rust
pub fn invert(s: &mut String) {
}
```

Write associated tests.

### Exercise 2

Write a function that converts an `i32` into a `String`, **without using
built-in facilities on string**. Start by handling positive integers, then
transition to all integers.

```rust
pub fn int_to_string(i: i32) -> String {
}
```

Write associated tests (same structure as previous exercize).

### Exercise 3

Write a function that converts a `String` into an `i32`, with associated
tests.

```rust
fn string_to_int(s: String) -> i32 {
}
```

### BONUS Exercise 4

Write a function that converts an `i32` into a `String` using
[Roman numerals](https://en.wikipedia.org/wiki/Roman_numerals).

Write the inverse function. Write associated tests.

```rust
pub fn int_to_string_roman(i: i32) -> String {
}

pub fn string_to_int_roman(i: String) -> i32 {
}
```

### Exercise 5

Write a stack type, that implements a stack of integers with a fixed max size
(using an array), with associated `pop`, `push`, and `peek` operations.

```rust
pub struct Stack {
    ...
}

impl Stack {
    fn new() -> Self {
        ...
    }

    fn pop(&mut self) -> Option<i32> {
        ...
    }

    fn push(&mut self, value : i32) -> bool {
        ...
    }

    fn peek(&mut self) -> Option<i32> {
        ...
    }
}
```

Write associated tests

### Exercise 6

Make the stack in exercise 5 be of unbounded size.

Write associated tests

### Exercise 7

Take the stack of exercise 6, and put it in a module:

```rust
```rust
mod stack {
    pub struct Stack {
        ...
    }

    impl Stack {
        ...
    }
}
```

Adapt the code so that tests pass again.

# Exercise 8

Implement an eval method for the `Expr` struct

```rust
#[derive(Debug)]
enum Operator {
    Plus, Minus, Divide, Multiply
}

#[derive(Debug)]
enum Expr {
    BinOp { l: Box<Expr>, op: Operator, r: Box<Expr> },
    IfExpr { cond: Box<Expr>, true_branch: Box<Expr>, false_branch: Box<Expr> },
    Literal(i32)
}

impl Expr {
    fn eval(&self) -> i32 {
        ...
    }
}
```

Write associated tests
