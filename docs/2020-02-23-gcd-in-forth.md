---
layout: post
author: Timmy Jose
title: "A simple GCD implementation in gforth"
date: 2020-02-23
---

I have been working through [gforth's](http://www.complang.tuwien.ac.at/forth/gforth/Docs-html/Tutorial.html#Tutorial) excellent Forth tutorials, and while it can be terse (almost cryptic) in places, I have found it rather smooth sailing so far. 
Given that Starting Forth is the usual book recommended to beginners, this may be a surprising choice. My reasoning is quite clear - I plan to work through both 
"Starting Forth" as well as "Thinking Forth". In fact, I had actually begun with the former, but quickly realised that the book's age did not lend itself too well to a 
modern Forth interpreter. Even so, the gforth manuals have been excellent so far.

In any case, one of the exercises in this tutorial series was to write a simple GCD calculator. So here is my solution, and I am rather pleased to be writing actual Forth code at last! Possibly Forth suits some quirk in my brain. Heh.

```
  : gcd   ( u1 u2 -- gcd )  
    begin
      dup 0<> while
        2dup < if swap then
        2dup mod rot drop
    repeat
    drop ;

```

Sample run:

```
18 24 gcd . 6  ok
24 18 gcd . 6  ok
24 17 gcd . 1  ok
12 36 gcd . 12  ok
10 10 gcd . 10  ok

```

Interestingly enough, the Forth compiler seems to convert the inner `if` to a `while`:

```
see gcd
: gcd
  BEGIN  dup 0<>
  WHILE  2dup <
  WHILE  swap
         THEN
         2dup mod rot drop
  REPEAT
  drop ; ok
```

[>](2020-03-04-recursive-gcd-in-forth.html)
[Home](/index.html)