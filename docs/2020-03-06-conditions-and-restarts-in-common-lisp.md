---
layout: post
author: Timmy Jose
title: "Conditions and Restarts in Common Lisp: A basic example"
date: 2020-04-06
---

This example is a (slightly) extended version of the example that I had written while discussing the topic of error handling in Common Lisp with someone on Reddit.

I thought that I could perhaps extend the given example into a basic (and yet minimal) program that demonstrates how the Conditions and Restarts system in Common Lisp operates. Of course, the system itself is so flexible that this is but one approach to structuring the code and semantics of error handling for the given example.

Consider a hypothetical example where we are working with some hardware (such as USB drives and hard drives), and we wish to capture the notion that they could possibly fail. 
Say in the case of a USB failure we have a set of possible actions we'd like to take, and likewise in the case of hard-drive failure.

Let's capture this scenario in some code:

First, let's define two error types, one each for USBs and hard drives:

````commonlisp
;;; the error types

(define-condition usb-unplugged (error)
  ((text
    :initarg :text
    :reader text)))

(define-condition hard-drive-crashed (error)
  ((text
    :initarg :text
    :reader text)))
```

Simple enough. Now, for the sake of simplicity, let's have, at the lowest-level of calls, a couple of functions that simply fail:

```commonlisp
;; low-level functions that fail

(defun cause-usb-error ()
  (error 'usb-unplugged
	 :text "the usb device was unplugged suddenly"))

(defun cause-hard-drive-error ()
  (error 'hard-drive-crashed
	 :text "this is an ex-hard drive!"))
```

At the next higher-level, we have some code that conditionally fails (or succeeds) - basically simulating interaction with USB and hard disk drives that could potentially fail:

```commonlisp
;; mid-level functions that can possibly fail

(defun do-some-usb-work (throw-error-flag)
  (if throw-error-flag
      (cause-usb-error)
      (format t "USB is formatted... all done!~%")))

(defun do-some-hard-drive-work (throw-error-flag)
  (if throw-error-flag
      (cause-hard-drive-error)
      (format t "Hard drive checked... all done!~%")))
```

Now, say these functions are actually part of a bigger workflow. This seems like a good place to define the possible "restarts" that we would like to have so as to handle the low-level errors that may occur:

```commonlisp
;;; upper-level code that calls functions that may fail

(defun do-some-job (throw-usb-error-flag throw-hard-drive-error-flag)
  (format t "I am chugging along nicely...~%")
  (restart-case
      (do-some-usb-work throw-usb-error-flag)
    (continue-without-caring ()
      (format t "Hahaha! USB doesn't matter... all done!~%"))
    (reset-flag-to-false ()
      (do-some-usb-work nil))
    (print-some-custom-message (message)
      (format t "~a~%" message))
    (exit ()
      nil))
  (restart-case
      (do-some-hard-drive-work throw-hard-drive-error-flag)
    (continue-without-caring ()
      (format t "Hahaha! Hard drive is irrelevant... all done!~%"))
    (reset-flag-to-false ()
      (do-some-hard-drive-work nil))
    (exit ()
      nil))
  (format t "Phew! Finally some peace and quiet.~%"))
```

And so for the case where we have USB failure, we have defined four possible ways to handle this error - continue, retry, print some custom message and continue, or simply exit. Likewise, for hard drive failures, we have three possible error handling strategies - continue, retry, or simply exit. Of course, these error-handling strategies may appear to be contrived and silly (which they are), but the main thrust of this code snippet is to show how we can separate the actual place where the error occurs from the place where we define the strategies themselves which is, in turn, separate from the place where we invoke the restarts from - complete modularity!

Finally, we simulate a client calling this function with a combination of all possible error-handling strategies:

```commonlisp
(defun main (fail-usb fail-hard-drive)
  (handler-bind
      ((usb-unplugged #'(lambda (c)
			  (format t "Got error: ~a~%" (text c))
			  (invoke-restart 'continue-without-caring)))
       (hard-drive-crashed #'(lambda (c)
			       (format t "Got error: ~a~%" (text c))
			       (invoke-restart 'continue-without-caring))))
    (do-some-job fail-usb fail-hard-drive)))
```

A couple of comments here - first off, it may appear that we are statically bound to the error-handling strategy we wish to adopt. About this, this is no necessarily true - we may have different variants of the `main` function above in different places that define different recovery strategies, may have a single function itself define multiple strategies dynamically, or any combination thereof. Moreover, these restarts would fit in nicely in interactive development (such as Emacs + SLIME). Secondly, `handler-bind` is not the only way to go about skinning this cat - we have a host of other "higher-level" handlers such as `restart-bind`, or we may simulate more traditional exception-handling using `handler-case` et al.

The example here presents, what I believe, a practical way to go about error-handling in general. For specific cases where this approach may not make sense, a different approach should be considered.

Whatever strategy/approach is taken, the guarantees that the Conditions and Restarts system provide will always hold true. For instance, that the stack will not be unwound automatically (making continuing or retrying actions much more useful).

For the given version of `main`:

```
CL-USER> (main nil nil)
I am chugging along nicely...
USB is formatted... all done!
Hard drive checked... all done!
Phew! Finally some peace and quiet.
NIL

CL-USER> (main nil t)
I am chugging along nicely...
USB is formatted... all done!
Got error: this is an ex-hard drive!
Hahaha! Hard drive is irrelevant... all done!
Phew! Finally some peace and quiet.
NIL

CL-USER> (main t nil)
I am chugging along nicely...
Got error: the usb device was unplugged suddenly
Hahaha! USB doesn't matter... all done!
Hard drive checked... all done!
Phew! Finally some peace and quiet.
NIL

CL-USER> (main t t)
I am chugging along nicely...
Got error: the usb device was unplugged suddenly
Hahaha! USB doesn't matter... all done!
Got error: this is an ex-hard drive!
Hahaha! Hard drive is irrelevant... all done!
Phew! Finally some peace and quiet.
NIL
```

If we were to have a different version of `main` such that we wish to always retry (successfully) for a USB drive and always exit for a hard drive error:

```commonlisp
(defun main (fail-usb fail-hard-drive)
  (handler-bind
      ((usb-unplugged #'(lambda (c)
			  (format t "Got error: ~a~%" (text c))
			  (invoke-restart 'reset-flag-to-false)))
       (hard-drive-crashed #'(lambda (c)
			       (format t "Got error: ~a~%" (text c))
			       (invoke-restart 'exit))))
    (do-some-job fail-usb fail-hard-drive)))
```

Some sample runs:

```
CL-USER> (main nil nil)
I am chugging along nicely...
USB is formatted... all done!
Hard drive checked... all done!
Phew! Finally some peace and quiet.
NIL

CL-USER> (main nil t)
I am chugging along nicely...
USB is formatted... all done!
Got error: this is an ex-hard drive!
Phew! Finally some peace and quiet.
NIL

CL-USER> (main t nil)
I am chugging along nicely...
Got error: the usb device was unplugged suddenly
USB is formatted... all done!
Hard drive checked... all done!
Phew! Finally some peace and quiet.
NIL

CL-USER> (main t t)
I am chugging along nicely...
Got error: the usb device was unplugged suddenly
USB is formatted... all done!
Got error: this is an ex-hard drive!
Phew! Finally some peace and quiet.
NIL
```

[<](2020-03-04-recursive-gcd-in-forth.html)
[Home](/index.html)
[>](2020-03-06-jlink-image-demo-helloworld.html)
