% Langages systèmes 5 - Advanced topics
% Raphael Amiard - Adacore
% amiard@adacore.com

# Ada & embedded programming

## Querying address

```ada
with System; use System;

package P is
   V : Integer;
   V_Addr : Address := V'Address;
   --                  ^ Address of V
end P;
```


## Querying alignment

```ada
package P is
   V : Integer;
   V_Alignment : Integer := V'Alignment;
   --                       ^ Alignment of V
end P;
```

## Querying size

```ada
package P is
   V : Integer;

   Typ_Size : Integer := Integer'Size;
   --                    ^ Minimum size for an Integer, in
   --                      bits (== 32)

   Typ_Size : Integer := Natural'Size;
   --                    ^ Minimum size for a Natural, in
   --                      bits (== 31)

end P;
```

## Querying size

```ada
package P is
   Instance_Size : Integer := V'Size;
   --                         ^ Actual size, in bits

   Typ_Size : Integer := Integer'Max_Size_In_Storage_Elements;
   --                    ^ Max size, in storage elements,
   --                      used to store an Int
   --                      (storage element -> byte)
end P;
```

## Specifying address

```ada
with System; use System;

package P is
   V : Integer;

   for V’Address
   use System.Storage_Elements.To_Address (16#fff_0000#);
   --                                      ^ Must be correctly aligned
end P;
```

## Specifying address

```ada
with System; use System;

package P is
   V : Integer
   with Address => System’To_Address (16#fff_0000#);
   --                    ^ GNAT specific attribute

   pragma Import (Ada, V);
   -- Prevents initialization
end P;
```

## Specifying address

```ada
procedure Pouet is
   A : array (1 .. 32) of Integer;

   B : array (1 .. 32 * 4) of Character
   with Address => A'Address;
   --  B is now an overlay for A, except you manipulate
   --  memory in bytes.

   type Rec is record
      A, B : Integer;
   end Rec;

   Inst : Rec;

   C : Integer
   with Address => Inst'Address;
begin
   null;
end Pouet;
```

## Specifying size

```ada
with System; use System;

package P is
   V : Natural
   with Size => 32;
   --           ^ Must be large enough.
   --           ^ Compiler can choose bigger size.
end P;
```

## Specifying alignment

```ada
with System; use System;

package P is
   V : Integer;
   with Alignment => 1;
   --                ^ Address must be a multiple of this.   
   --                  So compiler can over align.
end P;
```

## Packing arrays

```ada
procedure BV is
   type Bit_Vector is array (0 .. 31) of Boolean;
   pragma Pack (Bit_Vector);
   
   B : Bit_Vector;
begin
   Put_Line (Integer'Image (B'Size));
   -- Prints 32
end;
```

## Packing records

```ada
procedure Packed_Rec is
   type My_Rec is record
      A : Boolean;
      C : Natural;
   end record
   with Pack;

   R : My_Rec;
begin
   Put_Line (Integer'Image (R'Size));
   -- Prints 32
end Packed_Rec;
```

## Specifying record layout

```ada
type Register is range 0 .. 15;
   with Size => 4;
--  Size on type only affects components

type Opcode is (Load, Inc, Dec, ..., Mov);
   with Size => 8;

type RR_370_Instruction is record
   Code : Opcode;
   R1   : Register;
   R2   : Register;
end record;

for RR_370_Instruction use record
   Code at 0 range 0 .. 7;
   R1 at 1 range 0 .. 3;
   R2 at 1 range 4 .. 7;
end record;
```

## Bit to bit conversion

```ada
with Ada.Unchecked_Conversion;

procedure Unconv is
   subtype Str4 is String (1 .. 4);
   function To_Str4 is new Ada.Unchecked_Conversion (Integer, Str4);

   V : Integer;
   S : Str4;
   S := To_Str4 (V)
begin
   null;
end Unconv;
```

## Pragma Volatile

```ada
type Video_Buffer is array (Natural range <>) of RGB_Value;
pragma Volatile (Video_Buffer);
```

* Value might change at any time
* Compiler cannot optimize reads/writes, has to compile *exactly* each read and
  write the programmer means.


## Case study: Register overlays (1)

```ada
package Registers is

   type Bit    is mod 2 ** 1
     with Size => 1;
   type UInt5  is mod 2 ** 5
     with Size => 5;
   type UInt10 is mod 2 ** 10
     with Size => 10;

   subtype USB_Clock_Enable is Bit;
```

## Case study: Register overlays (2)

```ada
   type PMC_SCER_Register is record --  System Clock Enable Register
      Reserved_0_4   : UInt5            := 16#0#; -- Reserved bits
      USBCLK         : USB_Clock_Enable := 16#0#; -- Write-only. Enable USB FS Clock
      Reserved_6_15  : UInt10           := 16#0#; -- Reserved bits
   end record
     with
       Volatile,
       Size      => 16,
       Bit_Order => System.Low_Order_First;

   for PMC_SCER_Register use record
      Reserved_0_4   at 0 range 0 .. 4;
      USBCLK         at 0 range 5 .. 5;
      Reserved_6_15  at 0 range 6 .. 15;
   end record;

```

## Case study: Register overlays (3)

```ada
   type PMC_Peripheral is record --  Power Management Controller
      PMC_SCER       : aliased PMC_SCER_Register; -- System Clock Enable Register
      PMC_SCDR       : aliased PMC_SCER_Register; -- System Clock Disable Register
   end record
     with Volatile;

   for PMC_Peripheral use record
      PMC_SCER       at 16#0# range 0 .. 15; --  16-bit register at byte 0
      PMC_SCDR       at 16#2# range 0 .. 15; --  16-bit register at byte 2
   end record;

   --  Power Management Controller
   PMC_Periph : aliased PMC_Peripheral
     with Import, Address => System'To_Address (16#400E0600#);

end Registers;
```

# Rust macros

## Macro example

```rust
#[macro_export]
macro_rules! vec {
    ( $( $x:expr ),* ) => {
        {
            let mut temp_vec = Vec::new();
            $(
                temp_vec.push($x);
            )*
            temp_vec
        }
    };
}
```

## Macros

* Macros allow you to extend your language's syntax and semantics, by extending
  what it's able to do at compile time.

* Bad macros work on text (like C-like macros)

* Good macros work on structured input
    - In lisp, it worked on lists
    - In Rust, it works on ASTs, token trees, or token streams.

## When to use macros

* Never
* Never

## But seriously though

* Macros are used to:
    - Abstract common repetitive programming patterns
    - Embed domain specific languages
    - Provide lazy evaluation

* Generally: Macros are a last resort. Anything that you can solve another way
  shouldn't be fixed with macros.


## The rust parsing pipeline

First, tokenization: Turn string into tokens.

From there, Rust can either:

1. Produce a syntax tree from tokens
2. Produce a token tree that your macro can match upon

## Declarative macros

* Like the `vec` one.
* You use a form of pattern-matching on the arguments
* Used to provide function-like macros

## Declarative macros (2)

## Hygiene

```c
#define INCI(i) do { int a=0; ++i; } while (0)
int main(void)
{
    int a = 4, b = 8;
    INCI(a);
    INCI(b);
    printf("a is now %d, b is now %d\n", a, b);
    return 0;
}
```

## Hygiene and Rust



# Unsafe Rust

