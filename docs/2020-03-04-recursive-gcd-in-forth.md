---
layout: post
author: Timmy Jose
title: "A recursive GCD implementation in gforth"
date: 2020-03-04
---

In the previous [post](2020-02-23-gcd-in-forth.html), I presented an interative version of the GCD calculator. As an extension, here is the recursive
version of the same. 

This post also demonstrates the use of the `assert(` word which simply asserts that the flag between the `assert(` and ')' words is true, failing which, the 
Forth system throws an assertion error.

Here is the program:

```
\ a simple gcd calculator using recursion

: not   ( bool -- bool )
  0= ;

: gcd   ( u1 u2 -- gcd )
  \ we do not support the case where both numbers are 0
  assert( over 0<> over 0<> or )
  dup 0= if drop else
  swap over mod recurse
  then 
  ;

```

Sample run:

```
$ gforth
Gforth 0.7.3, Copyright (C) 1995-2008 Free Software Foundation, Inc.
Gforth comes with ABSOLUTELY NO WARRANTY; for details type `license'
Type `bye' to exit
s" gcd.fs" included  ok
12 18 gcd . 6  ok
18 12 gcd . 6  ok
2 1024 gcd . 2  ok
23 929 gcd . 1  ok
1 0 gcd . 1  ok
0 1 gcd . 1  ok
0 0 gcd . gcd.fs:7: failed assertion
:8: assertion failed
0 0 >>>gcd<<< .
Backtrace:
$10E6E79A8 throw
$10E70ECA8 c(abort")
$10E73A308 (end-assert)
```

[<](2020-02-23-gcd-in-forth.html)
[Home](/index.html)