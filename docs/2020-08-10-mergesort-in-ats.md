---
layout: post
author: Timmy Jose
title: "Functional Merge Sort in ATS"
date: 2020-08-10
---

A simple, functional merge sort implementation for a custom list type in ATS:


```
#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

(* our custom list data type *)
datatype
list0 (a: t@ype) =
  | list0_nil (a) of ()
  | list0_cons (a) of (a, list0 (a)) 

(* helper function to print out an int list *)
fun
print_int_list (xs: list0 (int)): void = 
  case+ xs of
    | list0_nil () => print_newline ()
    | list0_cons (x, xs) => (
      print! (x, " ");
      print_int_list (xs)
    )

fun
{a: t@ype}
list0_length (xs: list0 (a)): int = 
  case+ xs of
    | list0_nil () => 0
    | list0_cons (_, xs) => 1 + list0_length (xs)

(* return a list of the first n items of the list *)
fun
{a: t@ype}
list0_take (
  n: int,
  xs: list0 (a)
    ): list0 (a) = 
  case+ xs of
    | list0_nil () => list0_nil ()
    | list0_cons (x, xs) => 
        ifcase
          | n = 0 => list0_nil ()
          | _ => list0_cons (x, list0_take (n - 1, xs))

(* return the list with the first n items dropped *)
fun
{a: t@ype}
list0_drop (
  n: int,
  xs: list0 (a)
    ): list0 (a) = 
    case+ xs of
      | list0_nil () => list0_nil ()
      | list0_cons (x, xs) =>
        ifcase
          | n = 0 => list0_cons (x, xs)
          | _ => list0_drop (n - 1, xs)

typedef
cmp (a: t@ype) = (a, a) -> bool

(* merge two ordered lists into a single ordered list *)
fun
{a: t@ype}
list0_merge (
  cmp: cmp (a),
  xs: list0 (a),
  ys: list0 (a)
    ): list0 (a) =
    case+ (xs, ys) of
      | (list0_cons (x, xs), list0_cons (y, ys)) => 
          if cmp (x, y) then
            list0_cons (x, list0_merge (cmp, xs, list0_cons (y, ys)))
          else
            list0_cons (y, list0_merge (cmp, list0_cons (x, xs), ys))
      | (list0_nil (), ys) => ys
      | (xs, list0_nil ()) => xs

(* mergesort for our custom list type *)
fun
{a: t@ype}
list0_mergesort (
  cmp: cmp (a),
  xs: list0 (a)
    ): list0 (a) = 
    ifcase
      | list0_length (xs) = 1 => xs
       | _ => list0_merge (cmp, list0_mergesort (cmp, left), list0_mergesort (cmp, right)) where {
         val n = list0_length (xs) / 2
         val left = list0_take (n, xs)
         val right = list0_drop (n, xs)
       }

implement main0 () = {
  val li0 = list0_cons (1, 
            list0_cons (100, 
            list0_cons (~10, 
            list0_cons (12, 
            list0_cons (3, 
            list0_cons (~212, 
            list0_cons (0, 
            list0_cons (11, 
            list0_cons (~99, 
            list0_cons (0,
            list0_cons (1, 
            list0_cons (1, list0_nil ()))))))))))))
  val () = print_int_list (li0)
  
  val sorted_list_asc = list0_mergesort<int> (lam (x, y) => x <= y, li0)
  val () = print_int_list (sorted_list_asc)

  val sorted_list_desc = list0_mergesort<int> (lam (x, y) => x >= y, li0)
  val () = print_int_list (sorted_list_desc)

  val el0 = list0_nil ()
  val sel0 = list0_mergesort<int> (lam (x, y) => x <= y, el0)
  val () = print_int_list (sel0)

  val el1 = list0_cons (1, list0_nil ())
  val sel1 = list0_mergesort<int> (lam (x, y) => x <= y, el1)
  val () = print_int_list (sel1)

  val el2 = list0_cons (100, list0_cons (~99, list0_nil ()))
  val sel2 = list0_mergesort<int> (lam (x, y) => x >= y, el2)
  val () = print_int_list (el2)
}

```

Testing it out:

```
$ patscc -D_GNU_SOURCE -DATS_MEMALLOC_LIBC -O3 -o mergesort mergesort.dats

$ ./mergesort
1 100 -10 12 3 -212 0 11 -99 0 1 1
-212 -99 -10 0 0 1 1 1 3 11 12 100
100 12 11 3 1 1 1 0 0 -10 -99 -212

1
100 -99
```

Note: The blank line in the output above is for the empty list case.


[<](2020-08-04-generic-bst-in-ats)
[Home](/index.html)
