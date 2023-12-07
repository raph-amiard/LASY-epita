Exercises
=========

# Exercise 1 (Ada)

Using `Ada.Streams.Stream_IO` and representation clauses, create a program that
is able to read and display an ELF header.

* You need to parse the fields up to the `e_machine` field.
* Use
  [this link](https://en.wikipedia.org/wiki/Executable_and_Linkable_Format#File_header)
  as a reference
* Use enum mapping for enums
* You can use the hexadecimal notation if you need
* Check
  [this link](https://learn.adacore.com/courses/intro-to-ada/chapters/standard_library_files_streams.html#stream-i-o)
  for a reference about how to deserialize from a file to a data structure.

# Exercise 2 (Rust)

Make a variadic macro that:
* Prints the *source* of every expression it is passed as an argument
* Prints the evaluation of the expression

Such as :

```rust
    print_and_execute!(3, 34, "hello", {let x = 34;  x});
```

prints

```
3 evals to 3
34 evals to 34
"hello" evals to hello
{let x = 34; x} evals to 34
```

# Exercise 3 (Rust) - BONUS

Using [this
pattern](https://veykril.github.io/tlborm/decl-macros/patterns/tt-muncher.html),
create a macro that takes an expression as an input, and outputs an expression
for your expression evaluator from the previous class.
