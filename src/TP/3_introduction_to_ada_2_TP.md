Exercises
=========

### Exercise 1

Create an alire project named `ex1`. Given the following package definition:

```ada
package Expr_Eval is

    type Expr;

    type Expr_Kind is (Bin_Op, Literal, If_Expr);
    type Op_Kind is (Add, Sub, Mul, Div, Logic_And, Logic_Or);
    type Expr_Access is access Expr;

    type Expr (Kind : Expr_Kind) is record
      case Kind is
         when Bin_Op =>
            L, R : Expr_Access;
            Op   : Op_Kind;
         when If_Expr =>
            Cond, Then_Expr, Else_Expr : Expr_Access;
         when Literal =>
            Val : Integer;
      end case;
    end record;

    function Eval (E: Expr) return Integer;

end Expr_Eval;
```

Complete it with a body, in the file `expr_eval.adb`.

Write a test for it in the file `ex1.adb`. Here is a sample test (write more than this one).

```ada
with Expr_Eval; use Expr_Eval;

procedure Ex1 is
    E : Expr := (Kind => Bin_Op,
                 L => new Expr'(Kind => Literal, Val => 12),
                 R => new Expr'(Kind => Literal, Val => 15),
                 Op => Add)
begin
    Put_Line (Eval (E)'Image);
end Ex1;
```

### Exercise 2

Create an alire project named `ex2`. Transform exercise 1 to use a tagged type
hierarchy instead of a discriminated record. Here is the spec for `Expr_Eval`:

```ada
package Expr_Eval is

    type Expr;

    type Op_Kind is (Add, Sub, Mul, Div, Logic_And, Logic_Or);
    type Expr_Access is access Expr'Class;

    type Expr is abstract tagged null record;
    function Eval (E: Expr) return Integer is abstract;

    type Bin_Op is new Expr with record

    end record;
    overriding function Eval (B : Bin_Op) return Integer;

    type If_Expr is new Expr with record

    end record;
    overriding function Eval (I : If_Expr) return Integer;

    type Literal is new Expr with record

    end record;
    overriding function Eval (L : Literal) return Integer;

end Expr_Eval;
```

You can copy/adapt the test main and put it in `ex2.adb`.

Here is an example of initialization of the tagged record tree (change the
field names accordingly):

```ada
   E : Expr_Access := new Bin_Op'
     (K => Add, L => new Literal'(Val => 12), R => new Literal'(Val => 15));
```

### Exercise 3

Create an alire project named `ex3`.

Extend your prefered version to handle two more expression kinds:

- `Let`. The let expression allows the user to introduce a temporary
  binding from a name to a value.
- `Ref`. Ref allows referencing a name, introduced by a let, and the result
  of the evaluation will be the value of the binding.

You can assume that the same variable name cannot be used twice (you can just raise an exception or even not handle this case).

```ada
    subtype Var_Name is String (1 .. 2);
    type Let is new Expr with record
       Bind        : Var_Name;
       Bound_Value : Expr_Access;
       Target_Expr : Expr_Access;
    end record;
    overriding function Eval (L : Let) return Integer;

    type Ref is new Expr with record
       Ref : Var_Name;
    end record;
    overriding function Eval (R : Ref) return Integer;
```

> [!IMPORTANT]
>
> To represent the scopes, you can use either an array, or a hash map. Hash
> maps are in `Ada.Containers.Hashed_Maps`
>
> You can use https://learn.adacore.com/courses/intro-to-ada/chapters/standard_library_containers.html#hashed-maps as an example of how to use hash maps
> 

Write new tests in the test main for those two new constructs. Here is an
example of a Let according to the above data representation:

```ada
   E2 : Expr_Access :=
      new Let'
        ("Ab", new Literal'(Val => 12),
         new Bin_Op'(K => Add,
                     L => new Literal'(Val => 12),
                     R => new Ref'(Ref => "Ab")));
```


### Exercise 4 [BONUS]

Create an alire project named `ex4`. 

Copy over your version of exercise 3. Add a boolean literal. 

```ada
package Expr is
    type Expr;

    type Expr_Kind is (Bin_Op, Int_Literal, Bool_Literal, If_Expr);
    type Op_Kind is (Add, Sub, Mul, Div, Logic_And, Logic_Or);
    type Expr_Access is access Expr;

    type Expr_Val_Kind is (Bool_Kind, Int_Kind);

    type Expr_Val (Val_Kind : Expr_Val_Kind := Bool) is record
       case Kind is
          when Bool_Kind =>
             Bool_Val : Boolean;
          when Int_Kind =>
             Int_Val : Integer;
       end case;
    end record;

    type Expr (Kind : Expr_Kind) is record
      case Kind is
         when Bin_Op =>
            L, R : Expr_Access;
            Op   : Op_Kind;
         when If_Expr =>
            Cond, Then_Expr, Else_Expr : Expr_Access;
         when Literal =>
            Val : Expr_Val;
      end case;
    end record;

    function Eval (E: Expr) return Expr_Val;
    procedure Type_Check (E: Expr);
end Expr;
```

Your type check function must verify that boolean operators are only
used on booleans, that the if expression's condition is of a boolean type, and
that arithmetic operators are only used on integers.

When finding an inconsistency, it must raise an `Constraint_Error` exception
ith text "inconsistent types".
