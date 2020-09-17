---
layout: post
author: Timmy Jose
title: "Lazy Primes in Idris"
date: 2020-09-16
---

I thought it would be an interesting experiment to compare the generation of primes, lazily, in both Haskell and Idris.

Haskell has lazy evaluation by default, and Idris does support it via the `Lazy` and `Force` primitives, and infinite data structures
using `Streams` etc.

Here is the code in Haskell (adapted from Graham Hutton's book, "Programming in Haskell" (2nd Edition)):

```haskell

module Main where

primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p : xs) = p : filter (\x -> x `mod` p /= 0) (sieve xs)

main :: IO ()
main = do ns <- getLine
          let n = read ns
              ps = take n primes in
              do sequence_ [putStr $ show p ++ " " | p <- ps]

```

Testing it out:

```bash

~/dev/timmyjose.github.io/testing_ground$ ghc -O3 -o HPrimes Primes.hs && time ./HPrimes < input.in
Loaded package environment from /Users/z0ltan/.ghc/x86_64-darwin-8.10.1/environments/default
2 3 5 7 11 13 17 19 23 29 ... <output elided>

real    0m0.796s
user    0m0.775s
sys     0m0.009s
```

And here is the Idris version:

```haskell
module Main 

import Data.Strings
import Data.Stream

-- for some reason, the Data.Stream module does not have 
-- this function built-in
filter : (a -> Bool) -> Stream a -> Stream a
filter p (x :: xs) = if p x 
                        then x :: filter p xs
                        else filter p xs

sieve : Stream Int -> Stream Int
sieve (p :: xs) = p :: filter (\x => x `mod` p /= 0) (sieve xs)

primes : Stream Int
primes = sieve (iterate (+1) 2)

main : IO ()
main = do ns <- getLine
          let n = stringToNatOrZ ns
          let ps = take n primes
          sequence_ [putStr $ show p ++ " " | p <- ps]
```

Running it:

```bash
~/dev/timmyjose.github.io/testing_ground$ idris2 -o IPrimes Primes.idr && time ./build/exec/IPrimes < input.in
2 3 5 7 11 13 17 19 23 29 ... <output elided>

real    0m0.712s
user    0m0.676s
sys     0m0.027s
```

The file `input.in` simply contains the number of primes to generate which is, in this case, 10000:

```
~/dev/timmyjose.github.io/testing_ground$ cat input.in
10000
```

Considering that Idris2 is still well before version 1.0, and is being worked on actively by a small number of core developers, it is still very impressive that Idris managed to get within the same order of performance!

Maybe a similar version written in ATS would be an interesting experiment!

EDIT: As pointed out by [rmanne](https://github.com/rmanne), the implementation was incorrect. I had mistakenly included the wrong version for both Haskell and Idris. The recursive
call to `sieve` is missing, and hence the massive performance figures due to it generating the completely incorrect output! Embarrassing mistake, and thanks to `rmanne` again!

With the fixed code, the input size has been cut down to 10000 to keep times under a second. Performance difference in this case is negligible between the Haskell and Idris version as can be 
seen in the updated figures.

[<](2020-09-01-matrix-operations-in-idris)
[Home](/index.html)

