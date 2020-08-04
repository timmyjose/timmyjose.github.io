---
layout: post
author: Timmy Jose
title: "A generic Binary Search Tree implementation in ATS"
date: 2020-08-04
---

The more I learn [ATS](http://www.ats-lang.org/), the more I love it. Never mind the naysayers and the detractors who claim it is an illogically complex language. So far, I've found it to be a very
logical and relatively consistent language. It also seems like a very intuitive language once you get the hang of the patterns in the language. Furthermore, it seems like a
very good candidate for writing compilers in with its excellent support of Algebraic Data Types, and of generics (templates).

The community is still small, but quite helpful and proactive. Queries on the [Google groups](https://groups.google.com/g/ats-lang-users/) are answered with alacrity, and the people
seem very technology-focused, which is always a good sign of a healthy software community.

The error messages in ATS can be a bit messy, and hopefully this will be addressed in future versions, but the following tool, recommended by Richard from the ATS lang Google group is quite helpful - [ats-acc](https://github.com/sparverius/ats-acc). It formats the error messages quite nicely, and also gives a nice indication of the line where the error was triggered. This has made life so much easier for me.

As a general exercise, I wanted to extend the BST example given in the official tutorials, and using the excellent `ifcase` suggestion by the creator of ATS himself, Hongwei Xi, here is the annotated version of the code. It should be pretty much self-explanatory:

NOTE: `a: t@ype` indicates a generic type/typeclass constraint. And the `{a: t@type}` syntax before the function name is how template types are introduced in ATS.

```
#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

(* A generic binary search tree type *)
datatype
bstree (a: t@ype) = 
  | E of ()
  | B of (bstree (a), a, bstree (a))

(* insert a value into the bst, maintaining bst properties *)
fun
{a: t@ype}
insert (t0: bstree (a), k0: a): bstree (a) = 
  case+ t0 of
    | E () => B (E (), k0, E ())
    | B (t1, k, t2) => 
      let val sgn = gcompare_val_val<a>(k0, k)
      in ifcase
         | sgn < 0 => B (insert (t1, k0), k, t2) 
         | sgn > 0 => B (t1, k, insert (t2, k0))
         | _ => t0
      end

(* search for a value in the bst *)
fun
{a: t@ype}
search (t0: bstree (a), k0: a): bool = 
  case+ t0 of
    | E () => false
    | B (t1, k, t2) =>
      let val sgn = gcompare_val_val<a>(k0, k)
      in ifcase
          | sgn < 0 => search (t1, k0)
          | sgn > 0 => search (t2, k0)
          | _ => true
      end

(* pre-order traversal with a custom callback function *)
fun
{a: t@ype}
preorder (t0: bstree (a), fwork: a -<cloref1> void): void = 
  case+ t0 of
    | E () => ()
    | B (t1, k, t2) => {
        val () = fwork (k)
        val () = preorder (t1, fwork)
        val () = preorder (t2, fwork)
    }

(* in-order traversal with a custom callback function *)
fun
{a: t@ype}
inorder (t0: bstree (a), fwork: a -<cloref1> void): void = 
  case+ t0 of
    | E () => ()
    | B (t1, k, t2) => {
        val () = inorder (t1, fwork)
        val () = fwork (k)
        val () = inorder (t2, fwork)
    }

(* post-order traversal with a custom callback function *)
fun
{a: t@ype}
postorder (t0: bstree (a), fwork: a -<cloref1> void): void = 
  case+ t0 of
    | E () => ()
    | B (t1, k, t2) => {
        val () = postorder (t1, fwork)
        val () = postorder (t2, fwork)
        val () = fwork (k)
    }

implement main0 () = {
  val ti0: bstree (int) = E ()
  val ti0 = insert (ti0, 10)
  val ti0 = insert (ti0, 1)
  val ti0 = insert (ti0, 2)
  val ti0 = insert (ti0, 11)
  val ti0 = insert (ti0, 3)
  val ti0 = insert (ti0, 3)
  val ti0 = insert (ti0, 5)
  val () = preorder (ti0, lam k => print! (k, " "))
  val () = println! ()
  val () = inorder (ti0, lam k => print! (k, " "))
  val () = println! ()
  val () = postorder (ti0, lam k => print! (k, " "))
  val () = println! ()
  val () = assertloc (search (ti0, 11))
  val () = assertloc (search (ti0, 100) = false)

  val si0: bstree (string) = E ()
  val si0 = insert (si0, "hello")
  val si0 = insert (si0, "world")
  val si0 = insert (si0, "nice")
  val si0 = insert (si0, "to")
  val si0 = insert (si0, "meet")
  val si0 = insert (si0, "you")
  val si0 = insert (si0, "again")
  val () = preorder (si0, lam k => print! (k, " "))
  val () = println! ()
  val () = inorder (si0, lam k => print! (k, " "))
  val () = println! ()
  val () = postorder (si0, lam k => print! (k, " "))
  val () = println! ()
  val () = assertloc (search (si0, "hello"))
  val () = assertloc (search (si0, "hallo") = false)
}
```

Testing it out:

```
$ patscc -DATS_MEMALLOC_LIBC -o gen_tree gen_tree.dats && ./gen_tree
10 1 2 3 5 11
1 2 3 5 10 11
5 3 2 1 11 10
hello again world nice meet to you
again hello meet nice to world you
again meet to nice you world hello

```


[<](2020-07-31-coin-change-ats-rust-zig)
[Home](/index.html)
