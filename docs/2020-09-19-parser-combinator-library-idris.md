---
layout: post
author: Timmy Jose
title: "A simple Parser Combinator library in Idris"
date: 2020-09-19
---

Monadic parsing is a form of parsing that combines lexing, parsing, and often evaluation in the same recursive process. Languages with strong algebraic data types (and
thereby good pattern-matching) are excellent candidates for monadic parsing. Interestingly, Rust has a popular parser-combinator library called `nom`, but using it is not as 
clean as in ML-like languages due to the syntax used by Rust (not to mention involving the less-than-great macro system of Rust).

This is a simple exercise in implementing one of my favourite chapters in any programming textbook, the chapter on Monadic Parsing in the book, "Programming in Haskell" (2nd Edition)
by Professor Graham Hutton. He has written quite a few papers on the subject which are well worth reading as well. For instance, [Monadic Parser Combinators](https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf), and [Monadic Parsing in Haskell](https://www.cs.nott.ac.uk/~pszgmh/pearl.pdf) to mention a couple.

The basic idea here is to build up a relatively powerful parser-combinator library bottom-up - whereby we define a custom parser type, implement primitive parsers using the Monad andAlternative macghinery, and progressively build up more and more powerful parsers. 

Let's start off with the basic parser implementation:

```haskell

module ParsCom

import Data.Strings

%default total

{-
  Note that we get some parsers for free - 
   
   * A parser that always succeeds - `pure` from the Applicative interface
   * A parser that is equivalent to epsilon (no choice) - `empty` from the Alternative interface
   * sequencing by using `do` from the Monad interface
   * Choice by using the `(<|>)` operator from the Alternative interface
   * Repetitions by using the `some` and `many` functions (that we define ourselves since the Alternative
     interface in Idris, unlike Haskell, does not seem to have them)
-}

||| The Parser data type - takes a string argument 
||| and produces a result and the unconsumed string
public export
data Parser : Type -> Type where
  MkParser : (String -> List (a, String)) -> Parser a

||| Helper function to apply a parser
export
parse : Parser a -> String -> List (a, String)
parse (MkParser p) inp = p inp

||| A parser that consumes a single character
export
item : Parser Char
item = MkParser (\inp => case (unpack inp) of
                            [] => []
                            (c :: cs) => [(c, pack cs)])

--- making the Parser types a monad so that we can 
--- use the `do` notation

public export
Functor Parser where
  -- fmap : (a -> b) -> Parser a -> Parser b
   map f p = MkParser (\inp => case parse p inp of
                                    [(v, out)] => [(f v, out)]
                                    _ => [])

public export
Applicative Parser where
  -- pure : a -> Parser a
  pure v = MkParser (\inp => [(v, inp)])

  -- (<*>) : Parser (a -> b) -> Parser a -> Parser b
  pf <*> p = MkParser (\inp => case parse pf inp of
                                  [(f, out)] => parse (map f p) out
                                  _ => [])

public export
Monad Parser where
  -- (>>=) : Parser a -> (a -> Parser b) -> Parser b
  p >>= f = MkParser (\inp => case parse p inp of
                                  [(v, out)] => parse (f v) out
                                  _ => [])

-- make the Parser an instance of the Alternative class so that 
-- we can use choice and epsilon (no choice) operators

public export
Alternative Parser where
  -- empty : Parser a
  empty = MkParser (\_ => [])

  -- (<|>) : Parser a -> Parser a -> Parser a
  p <|> q = MkParser (\inp => case parse p inp of
                                [(v, out)] => [(v, out)]
                                _ => parse q inp )

-- primitive parsers

||| A parser that matches against the supplied predicate
sat : (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then pure x else empty

||| A parser that consumes a single character matching the supplied character
char : Char -> Parser Char
char c = sat (== c)

||| A Parser that matches against a digit character
digit : Parser Char
digit = sat isDigit

||| A Parser that matches against a lowercase letter
lower : Parser Char
lower = sat isLower

||| A Parser that matches against an uppercase letter
upper : Parser Char
upper = sat isUpper

||| A Parser that matches against an alphabetic character
alpha : Parser Char
alpha = sat isAlpha

||| A Parser that matches against an alphanumeric character
alphanum : Parser Char
alphanum = sat isAlphaNum

-- derived parsers

mutual
  export
  partial
  some : Parser a -> Parser (List a)
  some p = pure (::) <*> p <*> many p

  partial
  many : Parser a -> Parser (List a)
  many p = some p <|> pure []

||| A Parser that matches against whitespace
export
partial
space : Parser ()
space = do many (sat isSpace)
           pure ()

||| A Parser for a string of alphanumeric characters
partial
ident : Parser String
ident = do c <- item
           cs <- many alpha
           pure $ pack (c :: cs)

||| A Parser for an Nat value
partial
nat : Parser Nat
nat = do ns <- some digit
         pure (stringToNatOrZ (pack ns))

||| A Parser for an Int value
partial
int : Parser Int
int = do char '-'
         ns <- nat
         pure (-(cast ns))
      <|> do ns <- nat
             pure (cast ns)

||| A Parser that matches the supplied string
export
partial
string : String -> Parser String
string "" = pure ""
string cs = let (c :: cs) = unpack cs in
                do char c
                   string (pack cs)
                   pure $ pack (c :: cs)

-- higher-level parsers

||| A Parser that takes another Parser and handles spacing before and after
export
partial
token : Parser a -> Parser a
token p = do space
             v <- p
             space
             pure v

||| A Parser for an identifier
export
partial
identifier : Parser String
identifier = token ident

||| A Parser for a natural number
export
partial
natural : Parser Nat
natural = token nat

||| A Parser for an integer
export
partial
integer : Parser Int
integer = token int

||| A Parser for a string
export
partial
symbol : String -> Parser String
symbol cs = token (string cs)

```

That's it! Well, of course, it's not going to be as efficient or generic as `Parsec`, for instance, but it is excellent for edificational purposes.

Now let's use this mini-library to define a parser-cum-evaluator for basic arithmetic expressions. Note that the only error handling provided is using the `Maybe` type family.
For any industrial strength parser-combinator library, error handling would be, I imagine, one of the most important aspects of the library.


```haskell

module ArithExpr

import ParsCom

{-
  EBNF grammar for arithmetic expressions:

  expr ::= term (+ expr | - expr |  epsilon)
  term ::= factor (* term | / term | epsilon)
  factor ::= ( expr ) | integer
  integer ::= -2 | -1 | 0 | 1 | 2 | 3 ...

-}

mutual
  partial
  expr : Parser Int
  expr = do t <- term
            do symbol "+"
               e <- expr 
               pure (t + e) 
             <|> do symbol "-"
                    e <- expr
                    pure (t - e)
             <|> pure t

  partial
  term : Parser Int
  term = do f <- factor
            do symbol "*" 
               t <- term 
               pure (f * t)
             <|> do symbol "/" 
                    t <- term
                    pure (if t == 0 then 0 else f `div` t) 
             <|> pure f

  partial
  factor : Parser Int
  factor = do symbol "("
              e <- expr
              symbol ")"
              pure e
           <|> integer

export
partial
eval : String -> Maybe Int
eval inp = case parse expr inp of
                [(v, "")] => Just v
                _ => Nothing

```

And finally, the runner:

```haskell

module ParsMain

import ArithExpr

partial
main : IO ()
main = do e <- getLine
          case eval e of 
               Just v => putStrLn (show v)
               Nothing => putStrLn "Invalid input"

```

Let's take it for a spin!

```bash

~/dev/timmyjose.github.io/testing_ground/nzo$ idris2 -o ParsMain ParsMain.idr && rlwrap ./build/exec/ParsMain
1 + 2 * 3
7
~/dev/timmyjose.github.io/testing_ground/nzo$ idris2 -o ParsMain ParsMain.idr && rlwrap ./build/exec/ParsMain
(1 + 2) * 3
9
~/dev/timmyjose.github.io/testing_ground/nzo$ idris2 -o ParsMain ParsMain.idr && rlwrap ./build/exec/ParsMain
(12 / 2) * 3 + 3 * (12 / (1 + 3))
27
~/dev/timmyjose.github.io/testing_ground/nzo$ idris2 -o ParsMain ParsMain.idr && rlwrap ./build/exec/ParsMain
(1 + 2
Invalid input
~/dev/timmyjose.github.io/testing_ground/nzo$ idris2 -o ParsMain ParsMain.idr && rlwrap ./build/exec/ParsMain
12 / 0
0
~/dev/timmyjose.github.io/testing_ground/nzo$ idris2 -o ParsMain ParsMain.idr && rlwrap ./build/exec/ParsMain
12 - (100 / 10) / (20 / 5)
10
~/dev/timmyjose.github.io/testing_ground/nzo$ idris2 -o ParsMain ParsMain.idr && rlwrap ./build/exec/ParsMain
(100 / 20 / 4)
20

```

Some notes: 

Compared to the Haskell version, there were a couple of minor adjustments I had to make:

   1). Idris `String`S are different from Haskell `String`S, and hence all the `pack`ing and `unpack`ing business.

   2). The `Alternative` interface in Idris doesn't have the `some` and `many` repetition functions as Haskell's
       `Alternative` typeclass does, and hence I had to handroll my own version.

   3). Again, owing to the in-progress nature of Idris, evaluating even simple expressions in the REPL quickly became
       extremely (and I mean extremely) slow. I had to finally resort to compiling the code and running it from `ParsMain`.
       There is quite a lot of scope for performance improvement in Idris in this regard.

   4). The totality checking of Idris really does help in many tangible ways - unlike in Haskell, it forces you to be more
       deeply aware of your own code, and to be able to justify why your code is `total` and `covering` (or not).

Overall, I quite enjoyed writing it, perhaps even more so than the Haskell version! :-)

[<](2020-09-16-lazy-primes-in-idris)
[Home](/index.html)
