---
layout: post
author: Timmy Jose
title: "A simple file copy program in ATS"
date: 2020-08-12
---

The more I learn of ATS, the more I'm fascinated by it. Notwithstanding the [talk](https://youtu.be/zt0OQb1DBko) which pokes sufficient (light-hearted) fun at ATS, I actually find it very logical, including the naming and organisation of files, libraries, and concepts. 
After the initial get-go, it's been a veritable pleasure learning ATS.

I'm halfway through the first book, and there are a few more to complete, but I can see myself using ATS for a wide variety of projects - all the way from low-level programming
to the very highest abstractions.

Here is a simple program which copies the input file to the output file. Note the use of `strptr_free` which is required by the ATS tupe checker. Omitting this would lead to a 
compile-time error since the value returned by `fileref_get_line_string` is a linear type (of string). Very nice!

```
#include "share/atspre_staload.hats"

fn
copy_file (
  inpath: string,
  outpath: string
    ): void = 
    let val infil_ref = fileref_open_exn (inpath, file_mode_r)
        val outfil_ref = fileref_open_exn (outpath, file_mode_w)
        fun
       loop (
        infil_ref: FILEref,
        outfil_ref: FILEref
           ): void = 
           if fileref_isnot_eof (infil_ref) then
               let val line = fileref_get_line_string (infil_ref)
                   val () = fprint (outfil_ref, line)
                   val () = fprint_newline (outfil_ref)
                   val () = strptr_free (line)
               in
                loop (infil_ref, outfil_ref)
               end
    in
      loop (infil_ref, outfil_ref);
      fileref_close (infil_ref);
      fileref_close (outfil_ref)
    end

implement main0 () = 
  copy_file ("hamlet.in", "hamlet.out")
```

Testing it out:

```bash
$ cat hamlet.in
To be, or not to be. That is the question!
Exeunt.

$ patscc -DATS_MEMALLOC_LIBC -flto -cleanaft -O3 -latslib -o copyfile copyfile.dats && ./copyfile

$ cat hamlet.out
To be, or not to be. That is the question!
Exeunt.
```

[<](2020-08-11-line-number-closure-in-ats)
[Home](/index.html)
