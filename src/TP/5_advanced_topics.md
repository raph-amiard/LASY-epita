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

Make a variadic macro that evaluates and print every expression it is passed as
an argument.

# Exercise 3 (Rust)

Enhance the `ok_or_return` macro to evaluate each expression in order.

# Exercise 4 (Rust) - BONUS

Using [this
pattern](https://veykril.github.io/tlborm/decl-macros/patterns/tt-muncher.html),
create a macro that takes s-expressions as an input, and outputs an expression
for your expression evaluator from the previous class.
