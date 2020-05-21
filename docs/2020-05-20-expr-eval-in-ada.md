---
layout: post
author: Timmy Jose
title: "A simple Arithmetic Expression evaluator in Ada"
date: 2020-05-20
---

I recently completed Alexander Shvets' book, "Beginning Ada - from Novice to Professional", and while it was in sore need of editing in quite a few places, it was a most 
excellent starter book.

My plan now is to complete the AdaCore [learning materials] (https://learn.adacore.com/courses/courses.html), and then move onto some interesting projects using Ada. I quite like
the language, and think that it is a very well designed language. Never mind the verbosity - I think the only other mainstream languages that rival Ada in readability that I know of are - Java, Python, Kotlin, and Zig. Rust would have made the list if it were not for its traits hell.

Despite Ada not even pretending to be a functional language, it has sum types/algebraic types/discriminated unions/tagged unions/whatever-you-like-to-call-it, which quite surprised me. Of course, with a special Ada twist. Sum types are called "variant records" in Ada, and this basically means that the implementation of a sum type is done via the general "discriminant" system. 

This means, in essence, that we can specialise the record to have different states based upon an enumerated type that we pass in (and which is checked by the compiler at both compile time as well as runtime). Rather elegant, if different.

Extending the example given in the AdaCore learning materials, here is my version of one of my perennial favourite examples for demonstrating sum types - a simple arithmetic expresson evaluator.

The whole code is available [here](https://github.com/timmyjose-projects/arithmetic-expression-evaluator-in-ada). Only the relevant portions are shown here.

The overall project structure is so:

```bash
~/dev/projects/eval_expr_ada$ tree
.
├── LICENCE.md
├── README.md
├── bin
│   ├── debug
│   └── release
├── conf
├── docs
├── eval.gpr
├── obj
│   ├── debug
│   └── release
└── src
    ├── eval.adb
    ├── eval.ads
    ├── eval_main.adb
    ├── my_custom_int.adb
    └── my_custom_int.ads

9 directories, 8 files
```

Here are the `eval.ads` and `eval.adb` files which contains the actual logic for the evaluator:

```ada
--- eval.ads
generic
   type T is private;
   with function Image (Item : T) return String;
   with function "=" (First_Item, Second_Item : T) return Boolean;
   with function "+" (First_Item, Second_Item : T) return T;
   with function "-" (First_Item, Second_Item : T) return T;
   with function "*" (First_Item, Second_Item : T) return T;
   with function "/" (First_Item, Second_Item : T) return T;
package Eval is
   type Expr;
   type Expr_Access is access Expr;

   type Expr_Type is (Add, Sub, Mul, Div, Val);

   type Expr (Kind : Expr_Type) is record
      case Kind is
         when Val =>
            Val : T;
         when others =>
            Left, Right : Expr_Access;
      end case;
   end record;

   procedure Show (E : in Expr);
   function Eval (E : in Expr) return T;
end Eval;


```

```ada
--- eval.adb
with Ada.Text_IO;

package body Eval is
   procedure Show (E : in Expr) is
   begin
      case E.Kind is
         when Add =>
            Ada.Text_IO.Put ("Add(");
         when Sub =>
            Ada.Text_IO.Put ("Sub(");
         when Mul =>
            Ada.Text_IO.Put ("Mul(");
         when Div =>
            Ada.Text_IO.Put ("Div(");
         when Val =>
            Ada.Text_IO.Put (Image (E.Val));
            return;
      end case;

      Show (E.Left.all);
      Ada.Text_IO.Put (", ");
      Show (E.Right.all);
      Ada.Text_IO.Put (")");
   end Show;

   function Eval (E : in Expr) return T is
   begin
      case E.Kind is
         when Val =>
            return E.Val;
         when Add =>
            return Eval (E.Left.all) + Eval (E.Right.all);
         when Sub =>
            return Eval (E.Left.all) - Eval (E.Right.all);
         when Mul =>
            return Eval (E.Left.all) * Eval (E.Right.all);
         when Div =>
            return Eval (E.Left.all) / Eval (E.Right.all);
      end case;
   end Eval;
end Eval;

```

and the `eval_main.adb` file which is the client code:

```ada
with Ada.Text_IO;
with Ada.Float_Text_IO;

with Eval;
with My_Custom_Int;

procedure Eval_Main is
   package Eval_Integer is new Eval (T => Integer, Image => Integer'Image,
      "=" => "=", "+" => "+", "-" => "-", "*" => "*", "/" => "/");

   package Eval_Float is new Eval (T => Float, Image => Float'Image,
      "=" => "=", "+" => "+", "-" => "-", "*" => "*", "/" => "/");

   -- works for custom types as well
   package Eval_My_Custom_Int is new Eval (T => My_Custom_Int.My_Custom_Int,
      Image => My_Custom_Int.Image, "=" => My_Custom_Int."=",
      "+" => My_Custom_Int."+", "-" => My_Custom_Int."-",
      "*" => My_Custom_Int."*", "/" => My_Custom_Int."/");

   -- 2 + 3 * 4
   E1 : constant Eval_Integer.Expr :=
     (Kind  => Eval_Integer.Add,
      Left  => new Eval_Integer.Expr'(Kind => Eval_Integer.Val, Val => 2),
      Right =>
        new Eval_Integer.Expr'
          (Kind  => Eval_Integer.Mul,
           Left  => new Eval_Integer.Expr'(Kind => Eval_Integer.Val, Val => 3),
           Right =>
             new Eval_Integer.Expr'(Kind => Eval_Integer.Val, Val => 4)));

   -- 10 / 2 - 3
   E2 : constant Eval_Integer.Expr :=
     (Kind => Eval_Integer.Sub,
      Left =>
        new Eval_Integer.Expr'
          (Kind  => Eval_Integer.Div,
           Left => new Eval_Integer.Expr'(Kind => Eval_Integer.Val, Val => 10),
           Right =>
             new Eval_Integer.Expr'(Kind => Eval_Integer.Val, Val => 2)),
      Right => new Eval_Integer.Expr'(Kind => Eval_Integer.Val, Val => 3));

   -- 2.3 * 4.5 + 1.2
   E3 : constant Eval_Float.Expr :=
     (Kind => Eval_Float.Add,
      Left =>
        new Eval_Float.Expr'
          (Kind  => Eval_Float.Mul,
           Left  => new Eval_Float.Expr'(Kind => Eval_Float.Val, Val => 2.3),
           Right => new Eval_Float.Expr'(Kind => Eval_Float.Val, Val => 4.5)),
      Right => new Eval_Float.Expr'(Kind => Eval_Float.Val, Val => 1.2));

   -- 1.0 / 0.0
   E4 : constant Eval_Float.Expr :=
     (Kind  => Eval_Float.Div,
      Left  => new Eval_Float.Expr'(Kind => Eval_Float.Val, Val => 1.0),
      Right => new Eval_Float.Expr'(Kind => Eval_Float.Val, Val => 0.0));

   -- My_Custom_Int { Int_Val : 10 } * My_Custom_Int { Int_Val : 2 } / My_Custom_Int { Int_Val : 5 }
   E5 : constant Eval_My_Custom_Int.Expr :=
     (Kind => Eval_My_Custom_Int.Div,
      Left =>
        new Eval_My_Custom_Int.Expr'
          (Kind => Eval_My_Custom_Int.Mul,
           Left =>
             new Eval_My_Custom_Int.Expr'
               (Kind => Eval_My_Custom_Int.Val, Val => (Int_Val => 10)),
           Right =>
             new Eval_My_Custom_Int.Expr'
               (Kind => Eval_My_Custom_Int.Val, Val => (Int_Val => 2))),
      Right =>
        new Eval_My_Custom_Int.Expr'
          (Kind => Eval_My_Custom_Int.Val, Val => (Int_Val => 5)));

   -- My_Custom_Int { Int_Val : 100 } * My_Custom_Int { Int_Val : 2 } / My_Custom_Int { Int_Val : 5 }
   E6 : constant Eval_My_Custom_Int.Expr :=
     (Kind => Eval_My_Custom_Int.Div,
      Left =>
        new Eval_My_Custom_Int.Expr'
          (Kind => Eval_My_Custom_Int.Mul,
           Left =>
             new Eval_My_Custom_Int.Expr'
               (Kind => Eval_My_Custom_Int.Val, Val => (Int_Val => 100)),
           Right =>
             new Eval_My_Custom_Int.Expr'
               (Kind => Eval_My_Custom_Int.Val, Val => (Int_Val => 2))),
      Right =>
        new Eval_My_Custom_Int.Expr'
          (Kind => Eval_My_Custom_Int.Val, Val => (Int_Val => 5)));

begin
   Ada.Text_IO.Put ("The expression E1 is : ");
   Eval_Integer.Show (E1);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put
     ("And its value is " & Integer'Image (Eval_Integer.Eval (E1)));
   Ada.Text_IO.New_Line (2);

   Ada.Text_IO.Put ("The expression E2 is : ");
   Eval_Integer.Show (E2);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put
     ("And its value is " & Integer'Image (Eval_Integer.Eval (E2)));
   Ada.Text_IO.New_Line (2);

   Ada.Text_IO.Put ("The expression E3 is : ");
   Eval_Float.Show (E3);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put ("And its value is ");
   Ada.Float_Text_IO.Put (Eval_Float.Eval (E3), Aft => 3, Exp => 0);
   Ada.Text_IO.New_Line (2);

   Ada.Text_IO.Put ("The expression E4 is : ");
   Eval_Float.Show (E4);
   Ada.Text_IO.Put ("And its value is : ");
   Ada.Float_Text_IO.Put (Eval_Float.Eval (E4), Aft => 3, Exp => 0);
   Ada.Text_IO.New_Line (2);

   Ada.Text_IO.Put ("The expression E5 is : ");
   Eval_My_Custom_Int.Show (E5);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put ("And its value is : ");
   Ada.Text_IO.Put (My_Custom_Int.Image (Eval_My_Custom_Int.Eval (E5)));
   Ada.Text_IO.New_Line (2);

   declare
   begin
      Ada.Text_IO.Put ("The expression E6 is : ");
      Eval_My_Custom_Int.Show (E6);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("And its value is : ");
      Ada.Text_IO.Put (My_Custom_Int.Image (Eval_My_Custom_Int.Eval (E6)));
      Ada.Text_IO.New_Line (2);
   exception
      when Constraint_Error =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "Range check failed - too big a value for the My_Custom_Int type");
   end;
end Eval_Main;

```

Sample run:

```bash
~/dev/projects/eval_expr_ada$ gprbuild -Xmode=release -P eval.gpr
Compile
   [Ada]          eval_main.adb
   [Ada]          eval.adb
eval.ads:4:18: warning: function "=" is not referenced
   [Ada]          my_custom_int.adb
my_custom_int.ads:1:06: warning: unit "Eval" is not referenced
Bind
   [gprbind]      eval_main.bexch
   [Ada]          eval_main.ali
Link
   [link]         eval_main.adb
~/dev/projects/eval_expr_ada$ bin/release/eval_main
The expression E1 is : Add( 2, Mul( 3,  4))
And its value is  14

The expression E2 is : Sub(Div( 10,  2),  3)
And its value is  2

The expression E3 is : Add(Mul( 2.30000E+00,  4.50000E+00),  1.20000E+00)
And its value is 11.550

The expression E4 is : Div( 1.00000E+00,  0.00000E+00)And its value is : +Inf**

The expression E5 is : Div(Mul(My_Custom_Int { Int_Val =  10 }, My_Custom_Int { Int_Val =  2 }), My_Custom_Int { Int_Val =  5 })
And its value is : My_Custom_Int { Int_Val =  4 }

The expression E6 is : Div(Mul(My_Custom_Int { Int_Val =  100 }, My_Custom_Int { Int_Val =  2 }), My_Custom_Int { Int_Val =  5 })
And its value is : Range check failed - too big a value for the My_Custom_Int type
```

[<](2020-05-08-grep-in-ada)
[Home](/index.html)
