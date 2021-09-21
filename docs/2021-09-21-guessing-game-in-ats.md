---
layout: post
author: Timmy Jose
title: "Guessing Game in ATS"
date: 2021-09-21
---

Here is the quintessential guessing game as implemented in ATS.

The `SATS` file (the public external specification)

```
(* guessing_game.sats *)

fun play(): void

```

and the `DATS` file (the implementation):

```
(* guessing_game.dats *)

#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

staload "guessing_game.sats"

staload STDLIB = "libats/libc/SATS/stdlib.sats"
staload UN = "prelude/SATS/unsafe.sats"
staload TIME = "libats/libc/SATS/time.sats"

fun generate_secret
  (lower: int, upper: int): int =
  let 
    val () = $STDLIB.srand48($UN.cast{lint}($TIME.time_get()))
    val range = $UN.cast{lint}(upper - lower + 1)
    val random = $STDLIB.lrand48() mod range + $UN.cast{lint}(lower)
  in
    $UN.cast{int}(random)
  end

fun read_guess(): int = 
  let
    macdef MAXLEN = 20
    var inp = @[char][MAXLEN]()
    val () = print!("Enter guess: ")
    val bc = $extfcall(int, "scanf", "%s", addr@inp)
    val () = assertloc(bc != 0)
  in
    g0string2int($UN.cast{string}(addr@inp))
  end

fun play_aux
  (secret: int, guesses: int): void = 
  let
    val guess = read_guess()
  in
    ifcase
      | guess < secret => (println!("Too small! Try again..."); play_aux(secret, guesses + 1))
      | guess > secret => (println!("Too big! Try again..."); play_aux(secret, guesses + 1))
      | _ => 
          let
            val guesses = guesses + 1
          in
            println!("You win! You took ", guesses, " guesses.")
          end
  end

implement play() = 
  let
    val secret = generate_secret(1, 100)
    val guesses = 0
  in
    play_aux(secret, guesses)
  end

implement main0() = play()

```

and finally, the `Makefile`:

```
ATSFLAGS := -DGNU_SOURCE -DATS_MEMALLOC_LIBC -cleanaft -flto -O3 -latslib

GUESSING_GAME_SRC := guessing_game.sats guessing_game.dats
GUESSING_GAME_OBJ := ${GUESSING_GAME_SRC}
GUESSING_GAME_OBJ := ${patsubst %.sats, %_sats.o, ${GUESSING_GAME_OBJ}}
GUESSING_GAME_OBJ := ${patsubst %.dats, %_dats.o, ${GUESSING_GAME_OBJ}}

guessing_game: ${GUESSING_GAME_OBJ}
	acc pc ${ATSFLAGS} -o $@ $^

%_sats.o: %.sats
	acc pc -c $< || touch $@

%_dats.o: %.dats
	acc pc -DATS_MEMALLOC_LIBC -c $< || touch $@

clean:
	rm -f *.o *_?ats.c guessing_game

```

Running it:

```bash
$ make clean && make && ./guessing_game
rm -f *.o *_?ats.c guessing_game
acc pc -c guessing_game.sats || touch guessing_game_sats.o

------------------ C COMPILER MESSAGES ------------------

clang: warning: argument unused during compilation: '-L/Users/z0ltan/dev/forks/ATS2-Postiats/ccomp/atslib/lib' [-Wunused-command-line-argument]

-------------- END C COMPILER MESSAGES ------------------

acc pc -DATS_MEMALLOC_LIBC -c guessing_game.dats || touch guessing_game_dats.o

------------------ C COMPILER MESSAGES ------------------

clang: warning: argument unused during compilation: '-L/Users/z0ltan/dev/forks/ATS2-Postiats/ccomp/atslib/lib' [-Wunused-command-line-argument]

-------------- END C COMPILER MESSAGES ------------------

acc pc -DGNU_SOURCE -DATS_MEMALLOC_LIBC -cleanaft -flto -O3 -latslib -o guessing_game guessing_game_sats.o guessing_game_dats.o
Enter guess: 50
Too small! Try again...
Enter guess: 75
Too small! Try again...
Enter guess: 88
Too big! Try again...
Enter guess: 82
Too big! Try again...
Enter guess: 78
Too small! Try again...
Enter guess: 80
Too small! Try again...
Enter guess: 81
You win! You took 7 guesses.

```

[<](2021-09-13-functional-insertion-sort-in-ats)
[Home](/index.html)

