---
layout: post
author: Timmy Jose
title: "Guessing Game in LOGO"
date: 2021-01-31
---

The very first language that I ever learnt in my life was LOGO all the way back in the third grade (year/standard). I immediately fell in love with it, and I would eagerly wait for
the lab to open so that I could try out all the pictures that I had in mind - houses, spirals, animals, scenery and what not! The funny thing is that back then, as is the case today, LOGO has the slight misfortune of becoming synonymous with "Turtle Graphics". In fact, for decades after, I myself believed that that was all thet LOGO was good for.

Imagine my surprise when I read on some forum that LOGO actually was a *sort* of Lisp. I could not believe it, and when I started digging back into LOGO, I realised that this belief is not unfounded. In fact, in some ways, LOGO is a better Lisp than many Lisps available today! I've been learning LOGO for a few days now (not just for the sheer edificational
fun of it, but perhaps also for some more pracical reasons which may become more concrete in the near future), and in fact it almost feels like Forth done right - the natural function composition based on the arity of the functions (procedures as well) themselves is nothing short of simplistic beauty.

The book I have been using to (re)learn LOGO is "Symbolic Computing" by Brian Harvey. This is the first of a series of books (and all of which I plan to complete). The thing that stood out most for me is that difference that a good pedagogical bent of mind can make. This book, along with JIm Blandy and Jason Orendorff's "Programming Rust" and Graham Hutton's "Programming in Haskell" are probably my most favourite programming language books of all time.

So I made a simple guessing game (my go to example when learning a new language or just fooling around) in LOGO. It runs in UCB LOGO. Here is the code:

```
to guessing.game
  print [Welcome to the guessing game!]
  print [Your task is to guess a secret number between 1 and 100 (exclusive)]
  play get.secret 1 100 1
end

to play :secret :score
  print [Enter your guess:]
  local "guess
  make "guess first readlist
  if lessp :guess :secret [print [Too small! Try again!] play :secret :score+1]
  if greaterp :guess :secret [print [Too big! Try again!] play :secret :score+1]  
  if equalp :guess :secret ~
    [print (sentence [Correct! You took] :score [guesses. Thank you for playing.]) stop] 
end

to get.secret :low :high
  output sum random :high-:low :low
end

```

and a small session showing it in action:

![Demo](/assets/images/guessing-game-in-logo.png)

(Sorry for the image - I could not find a way to export the interactive session from UCB LOGO).

Happy Days!

[<](2020-09-19-parser-combinator-library-idris)
[Home](/index.html)
