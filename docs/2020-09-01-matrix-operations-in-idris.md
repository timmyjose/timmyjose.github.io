---
layout: post
author: Timmy Jose
title: "Basic matrix operations in Idris"
date: 2020-09-01
---

I had tried Idris some time back, and while I liked it, I didn't follow through with it. One major reason was the fact that Idris' focus appeared to be purely on research despite
marketing itself as a language for programmers. There is a bit of contradiction in that statement. Of late, however, especially with the ongoing rewrite of Idris in Idris (Idris2), as well as support for Linear Types, the general feeling appears to be that Idris2 is geared not only towards research but also for industry programming (there is still some way to goin this respect though).

Given that I am investing in [ATS](http://www.ats-lang.org/), which also has powerful support for Dependent Types as well as Linear Types, and quite a bit of theorem proving (albeit more on the imperative side), coupled with my longterm interest in Compilers, I decided to learn Idris and ATS together, with a special focus on creating compilers for my own languages. Compilers and Programming Languages are very appealing to me, always have been, and I have decided to invest in them for the long haul.

As part of restarting my study of Idris, I am working through the official book by Idris' creator, Edwin Brady - ["Type driven development with Idris"](https://www.manning.com/books/type-driven-development-with-idris), which is still valid (with minor changes). This book does not contain any material on Linear Types, of course, but that's where the [official docs](https://idris2.readthedocs.io/en/latest/) will come in handy.

I recall last time that I'd got stuck on the matrix multiplication example in the TDD book (and skipped it), and this time I decided to get over the hump for good. 

So here is a small module with some very basic matrix operations, all runnable in Idris2.

```haskell

module Matrix

import Data.Vect

%default total

||| Add two matrices together
addMatrix : Num elem => Vect m (Vect n elem) -> Vect m (Vect n elem) -> Vect m (Vect n elem)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys

||| Subtract the second matrix from the first
subMatrix : (Neg elem, Num elem) => Vect m (Vect n elem) -> Vect m (Vect n elem) -> Vect m (Vect n elem)
subMatrix [] [] = []
subMatrix (x :: xs) (y :: ys) = zipWith (-) x y :: subMatrix xs ys

||| Transpose a matrix
transposeMatrix : { n : _ } -> Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMatrix [] = replicate n []
transposeMatrix (x :: xs) = let xsTrans = transposeMatrix xs in
                                zipWith (::) x xsTrans

||| Multiply two matrices together
multiplyMatrix : Num elem => { m, n, o : _ } -> Vect m (Vect n elem) -> Vect n (Vect o elem) -> Vect m (Vect o elem)
multiplyMatrix xs ys = multiply xs (transposeMatrix ys)
  where
    dotProduct : { n : _ } -> Vect n elem -> Vect n elem -> elem
    dotProduct [] [] = 0
    dotProduct (x :: xs) (y :: ys) = x * y + dotProduct xs ys

    multHelper : { n, o : _ } -> Vect n elem -> Vect o (Vect n elem) -> Vect o elem
    multHelper _ [] = []
    multHelper x (y :: ys) = dotProduct x y :: multHelper x ys

    multiply : { m, n, o : _ } -> Vect m (Vect n elem) -> Vect o (Vect n elem) -> Vect m (Vect o elem)
    multiply [] _ = []
    multiply (x :: xs) ys = multHelper x ys :: multiply xs ys

```

Testing it out:

```
Matrix> addMatrix [[1, 2, 3], [4, 5, 6]] [[2, 1, 3], [6, 5, 4]]
[[3, 3, 6], [10, 10, 10]]

Matrix> subMatrix [[1, 2, 3], [4, 5, 6]] [[2, 1, 3], [6, 5, 4]]
[[-1, 1, 0], [-2, 0, 2]]

Matrix> multiplyMatrix [[1, 2], [3, 4], [5, 6]] [[1, 2, 3, 4], [2, 3, 4, 5]]
[[5, 8, 11, 14], [11, 18, 25, 32], [17, 28, 39, 50]]

Matrix> transposeMatrix [[1, 2], [3, 4], [5, 6], [7, 8]]
[[1, 3, 5, 7], [2, 4, 6, 8]]

```

Epilogue: One nice thing that I'd not observed before, and which appears blatantly obvious now, is how the use of dependent types makes pattern matching much simpler. For instance, in the `addMatrix` example, we don't have to consider every combination of the two matrices for the function to be `total`, since the dependent types ensure that the dimensions always
match up!

Note, however, also the extra verbosity in the type signatures due to the inclusion of the concept of "multiplicities" in Idris2. This means that implicit variables in Idris now havea multiplicity of `0`, and hence must be explicitly introduced into scope to be used in the local context. This is especially amplified when the definitions are local, as in this case. If the local functions in `multiplyMatrix` were lifted to the top-level, it would simplify the type signature to a considerable extent.

[<](2020-08-12-file-copy-in-ats)
[Home](/index.html)
[>](2020-09-16-lazy-primes-in-idris)
