Exercises
=========

## Introduction

> [!IMPORTANT]  
> 
> PLEASE FOLLOW THE FOLLOWING RULES CAREFULLY. They're needed to grade your
> submissions, and any deviation will be penalized

* Create one project for the whole TP with `cargo init tp4`
* Create one file per exercize, named `ex<N>.rs` where N is the number of the
  exercize (so `ex1.rs` for exercize 1)
* In the main, include `mod ex<N>` directives so that the ex files are
  compiled.


# Exercise 1

Create a generic `SortedList` type, where elements are sorted on insertion:

```rust
#[derive(Debug)]
struct SortedList<...> {
    ...
}

impl<...> SortedList<...> {
    pub fn add...
}
```

# Exercise 2

Take back your expression evaluator from last class

Use lifetime annotations and references inside your evaluator instead of boxes.

> [!IMPORTANT]  
> Don't use this version for further questions!

# Exercise 3

Take back your expression evaluator from last class

* Make it dynamically typed:
    - Add a boolean type to the values that can be represented by the evaluator
    - Add true & false literal
    - Add relational operators (and, or)

You need to declare a new enum type for values, and eval needs to return this
enum type.

```rust
#[derive(Debug, Eq, Clone)]
enum ExprResult {
    Bool(bool),
    Int(i32),
}
```

* Add a `let` expression and a `ref` expression:
    - `Let` allows the user to declare a local binding (binding a value to a name)
    - `Ref` allows the user to reference a binding, returning its value

[!NOTE]
TIP: You'll need to use hash maps to represent scopes, binding
names to values

```rust
fn eval(expr: Expr) -> ExprResult {
    let mut vars = HashMap::<String, ExprResult>::new();

    eval_impl(expr, &mut vars) // Implement eval_impl
}
```

# Exercise 4

Use https://docs.rs/lexpr/latest/lexpr/ to make a simple parser for your
expression evaluator. make a function that takes a string and returns an
expression, like:

```lisp
(let a 12 (+ a (if true 15 18)))
```

The following s-expressions should parse and evaluate:

```lisp
1
(if true 15 18)
(let a 12 (+ a (if true 15 18)))
(+ (* 12 14) (/ 4 2))
```

Here is the skeleton for the `parse` function:

```rust
use lexpr::{self, Parser, Value};

fn parse(value: &Value) -> Expr {
    ...
}
```

# Exercise 5

* Reimplement your evaluator using an `Expr` trait, and trait objects, rather
than an enum.

```rust
trait Expr {
    fn eval(&self, vars: &mut HashMap<String, ExprResult>) -> ExprResult;
}

struct IntLiteral {
    value: i32
}

impl Expr for IntLiteral {
    fn eval(&self, _vars: &mut HashMap<String, ExprResult>) -> ExprResult {
        ...
    }
}

// Implement other exprs
```

* Showcase the extensibility by adding a new expression in another module:
  `Print` will print the value of an expression and return it

# Exercise 6 (bonus)

Add a

```
enum ExprType { Bool, Int }

fn check_types(&self) -> Result<ExprType, 'static str>
```

function to the expression evaluator, which will return the type of an
expression before evaluation, *without* evaluating it.
