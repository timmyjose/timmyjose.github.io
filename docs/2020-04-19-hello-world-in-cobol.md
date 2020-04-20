---
layout: post
author: Timmy Jose
title: "A simple \"Hello, world\" program in COBOL"
date: 2020-04-19
---

Recently I decided to start learning some COBOL (never had it in university). I'm using the excellent book by Michael Coughlan, "Beginning COBOL for Programmers" - the author's 
vast teaching experience shows through in this excellent book.

Beyond COBOL, I plan to start learning about mainframes as well. Why? Well, why not?

In that vein, here is a simple "Hello, world" program in ANS COBOL (I'm using `gnucobol` on macOS):

  ```cobol
      *> Display "Hello, world" on the screen
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HelloWorld.
       AUTHOR. Timmy Jose.
       DATE-WRITTEN. 19th APR 2020.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 DisplayMessage PIC X(12) VALUE "Hello, world".
       PROCEDURE DIVISION.
       BEGIN.
           DISPLAY DisplayMessage
           STOP RUN.

  ```

Running it:

```bash
~/dev/playground$ cobc -x HelloWorld.cob && ./HelloWorld
Hello, world

```

[<](2020-03-06-jlink-image-demo-helloworld.html)
[Home](/index.html)
[>](2020-04-20-guessing-game-in-kotlin.html)
