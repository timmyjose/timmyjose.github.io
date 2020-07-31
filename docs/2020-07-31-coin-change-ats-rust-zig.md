---
layout: post
author: Timmy Jose
title: "The Coin Change problem in ATS, Rust, and Zig - a comparison"
date: 2020-05-29
---

I have decided to jump straight into the murky depths of [ATS](http://www.ats-lang.org/), and finally master it. It is a rather strange but powerful language, and despite its
sharp edges, I think it is a very interestingly designed language, and has quite a lot to offer up in terms of edificational value.

As part of working through the [official tutorials](http://ats-lang.sourceforge.net/DOCUMENT/INT2PROGINATS/HTML/book1.html), I came across a simple implementation of the
good old [coin changing problem ](http://ats-lang.sourceforge.net/DOCUMENT/INT2PROGINATS/HTML/x597.html).

I thought that it might be an interesting experiment to try out the ATS implementation against Rust and Zig. So here's the outcome. I don't think we should read much into it,
but I was more surprised at the comparable performance of the ATS version to the Rust and Zig ports. Also note that the code samples in Rust and Zig were written to match
the ATS version as faithfully as possible, and so may not be considered idiomatic by any stretch of the imagination.

The results are as follows - Zig was the fastest, followed by Rust, and finally ATS (well, the compiled generated C code that is).

Here is the original ATS version:

```
#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

typedef
int4 = (int, int, int, int)

val theCoins = (1, 5, 10, 25): int4

fn coin_get
  (n: int): int = 
 (if n = 0 then theCoins.0
  else if n = 1 then theCoins.1
  else if n = 2 then theCoins.2
  else if n = 3 then theCoins.3
  else ~1)

fun coin_change
  (sum: int): int =
  let
    fun aux (sum: int, n: int): int = 
      if sum > 0 then
        (if n >= 0 then aux (sum, n - 1) + aux (sum - coin_get (n), n) else 0)
      else (if sum < 0 then 0 else 1)
  in
    aux (sum, 3)
  end

implement 
main0 () = {
  val () = println! ("coin_change (25) = ", coin_change (25))
  val () = println! ("coin_change (100) = ", coin_change (100))
  val () = println! ("coin_change (1000) = ", coin_change (1000))
}

```

Doing a test run of 5 samples:

```bash
$ patscc -O2 -o  coin_change coin_change.dats
$ for i in {1..5}; do time ./coin_change; done
coin_change (25) = 13
coin_change (100) = 242
coin_change (1000) = 142511

real    0m0.238s
user    0m0.235s
sys     0m0.002s
coin_change (25) = 13
coin_change (100) = 242
coin_change (1000) = 142511

real    0m0.235s
user    0m0.232s
sys     0m0.002s
coin_change (25) = 13
coin_change (100) = 242
coin_change (1000) = 142511

real    0m0.237s
user    0m0.234s
sys     0m0.002s
coin_change (25) = 13
coin_change (100) = 242
coin_change (1000) = 142511

real    0m0.235s
user    0m0.232s
sys     0m0.001s
coin_change (25) = 13
coin_change (100) = 242
coin_change (1000) = 142511

real    0m0.235s
user    0m0.232s
sys     0m0.001s
```

Here is the Rust version:

```rust
fn coin_change(sum: isize, coins: &[isize]) -> isize {
    fn aux(sum: isize, n: isize, coins: &[isize]) -> isize {
        if sum > 0 {
            if n >= 0 {
                return aux(sum, n - 1, coins) + aux(sum - coins[n as usize], n, coins);
            } else {
                return 0;
            }
        } else {
            if sum < 0 {
                return 0;
            } else {
                return 1;
            }
        }
    }

    aux(sum, 3, coins)
}

fn main() {
    let coins = [1, 5, 10, 25];

    println!("coin_change(25) = {}", coin_change(25, &coins));
    println!("coin_change(100) = {}", coin_change(100, &coins));
    println!("coin_change(1000) = {}", coin_change(1000, &coins));
}

```

```bash
$ rustc -O -o coin_change coin_change.rs
$ for i in {1..5}; do time ./coin_change; done
coin_change(25) = 13
coin_change(100) = 242
coin_change(1000) = 142511

real    0m0.192s
user    0m0.187s
sys     0m0.003s
coin_change(25) = 13
coin_change(100) = 242
coin_change(1000) = 142511

real    0m0.187s
user    0m0.184s
sys     0m0.001s
coin_change(25) = 13
coin_change(100) = 242
coin_change(1000) = 142511

real    0m0.188s
user    0m0.185s
sys     0m0.001s
coin_change(25) = 13
coin_change(100) = 242
coin_change(1000) = 142511

real    0m0.187s
user    0m0.185s
sys     0m0.001s
coin_change(25) = 13
coin_change(100) = 242
coin_change(1000) = 142511

real    0m0.186s
user    0m0.184s
sys     0m0.002s

```

And finally the Zig version:

```
const std = @import("std");

const coins = [_]isize{ 1, 5, 10, 25 };

fn coinChange(sum: isize) isize {
    const S = struct {
        fn aux(s: isize, n: isize) isize {
            if (s > 0) {
                if (n >= 0) {
                    return aux(s, n - 1) + aux(s - coins[@intCast(usize, n)], n);
                } else {
                    return 0;
                }
            } else {
                if (s < 0) {
                    return 0;
                } else {
                    return 1;
                }
            }
        }
    };

    return S.aux(sum, 3);
}

pub fn main() !void {
    const stdout = std.io.getStdOut().outStream();
    try stdout.print("coin_change(25) = {}\n", .{coinChange(25)});
    try stdout.print("coin_change(100) = {}\n", .{coinChange(100)});
    try stdout.print("coin_change(1000) = {}\n", .{coinChange(1000)});
}

```

```bash
$ zig build-exe --release-fast coin_change.zig
$ for i in {1..5}; do time ./coin_change; done
coin_change(25) = 13
coin_change(100) = 242
coin_change(1000) = 142511

real    0m0.129s
user    0m0.125s
sys     0m0.002s
coin_change(25) = 13
coin_change(100) = 242
coin_change(1000) = 142511

real    0m0.126s
user    0m0.123s
sys     0m0.001s
coin_change(25) = 13
coin_change(100) = 242
coin_change(1000) = 142511

real    0m0.123s
user    0m0.121s
sys     0m0.001s
coin_change(25) = 13
coin_change(100) = 242
coin_change(1000) = 142511

real    0m0.123s
user    0m0.121s
sys     0m0.001s
coin_change(25) = 13
coin_change(100) = 242
coin_change(1000) = 142511

real    0m0.123s
user    0m0.120s
sys     0m0.001s
```

[<](2020-05-29-expr-eval-in-zig)
[Home](/index.html)
