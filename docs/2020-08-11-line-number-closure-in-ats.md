---
layout: post
author: Timmy Jose
title: "A line number closure implementation in ATS"
date: 2020-08-11
---

This is an implementation, in ATS, of one of my favourite exercises in closures - a [line-number printing closure](https://z0ltan.wordpress.com/2017/01/25/hoytes-line-number-closure-in-a-few-choice-languages/). 

For reference, the Rust version is given first, and then the same in ATS, whih has a unique flavour of its own.

Here is the Rust version:

```rust
fn linum() -> impl FnMut() -> () {
    let mut line = 0;

    Box::new(move || {
        line = line + 1;
        println!("Current line number is {}", line);
    })
}

fn main() {
    let mut l0 = linum();
    for _ in 0..5 {
        l0();
    }

    l0 = linum();
    l0();
    l0();
    l0();
}
```

Running it:

```
$ rustc -O linum.rs && ./linum
Current line number is 1
Current line number is 2
Current line number is 3
Current line number is 4
Current line number is 5
Current line number is 1
Current line number is 2
Current line number is 3

```

And here is the same in ATS:


```
#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

fn
linum () :<cloref1> () -<cloref1> void = 
  let val line = ref<int> (0)
  in
    lam () =<cloref1> (
      !line := !line + 1;
      println! ("Current line number is ", !line)
    )
  end

fun loop (li0: () -<cloref1> void, n: int): void = 
  if n > 0 then (li0 (); loop (li0, n - 1))

implement main0 () = {
  val li0 = linum ();
  val () = loop (li0, 5)

  val li0 = linum ()
  val () = li0 ()
  val () = li0 ()
  val () = li0 ()
}
```

Testing it out:

```
$ patscc -DATS_MEMALLOC_LIBC -O3 -o linum linum.dats && ./linum
Current line number is 1
Current line number is 2
Current line number is 3
Current line number is 4
Current line number is 5
Current line number is 1
Current line number is 2
Current line number is 3
```

[<](2020-08-10-mergesort-in-ats)
[Home](/index.html)
