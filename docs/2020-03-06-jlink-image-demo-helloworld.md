---
layout: post
author: Timmy Jose
title: "Creating an image file using jlink - a basic tutorial"
date: 2020-04-06
---

Let's explore how to create a minimal self-hosted image using the module system of Java and the `jlink` tool.

First, for reference, I am using OpenJDK 11:

```bash 
openjdk version "11.0.6" 2020-01-14
OpenJDK Runtime Environment (build 11.0.6+10)
OpenJDK 64-Bit Server VM (build 11.0.6+10, mixed mode)
```

Let's create a directory for our little demo:

```bash
~/dev/playground$ mkdir helloworld_jlink
~/dev/playground$ cd helloworld_jlink/
~/dev/playground/helloworld_jlink$ tree .

0 directories, 0 files
```

Now, let's create the module for our demo:

````bash
~/dev/playground/helloworld_jlink$ mkdir com.foo.bar
~/dev/playground/helloworld_jlink$ vim com.foo.bar/module-info.java
~/dev/playground/helloworld_jlink$ tree
.
└── com.foo.bar
    └── module-info.java

1 directory, 1 file

~/dev/playground/helloworld_jlink$ cat com.foo.bar/module-info.java
module com.foo.bar {

}
```

Here, we have `com.foo.bar` as the name of our new module. The `module-info.java` file inside the `com.foo.bar` directory simply makes its parent directory a module. Okay so far?

Now, let's create our `helloWorld` class inside the package `com.foo.bar` (don't be confused - this `com.foo.bar` is a package inside the `com.foo.bar` module):

```bash
~/dev/playground/helloworld_jlink$ cd com.foo.bar/
~/dev/playground/helloworld_jlink/com.foo.bar$ mkdir -p com/foo/bar
~/dev/playground/helloworld_jlink/com.foo.bar$ vim com/foo/bar/HelloWorld.java
~/dev/playground/helloworld_jlink/com.foo.bar$ cat com/foo/bar/HelloWorld.java
package com.foo.bar;

public class HelloWorld {
  public static void main(String[] args) {
    System.out.println("Hola, Mundo!");
  }
}
```

At this point, our directory structure looks like so:

```bash
~/dev/playground/helloworld_jlink/com.foo.bar$ cd ..
~/dev/playground/helloworld_jlink$ tree
.
└── com.foo.bar
    ├── com
    │   └── foo
    │       └── bar
    │           └── HelloWorld.java
    └── module-info.java

4 directories, 2 files
```

Excellent. Now, let's compile our class and generate a JAR file (note that jlink works with JAR files or a specific format called the JMOD format, which is quite useful in including native code and other non-Java resources. For this example, let's stick to a JAR file):

By convention, let's keep our compiled classes in a directory called `mods` within which we also create a directory for the module name (since we might have more than one module in a project), so we create it first:

```bash
~/dev/playground/helloworld_jlink$ mkdir -p mods/com.foo.bar
~/dev/playground/helloworld_jlink$ tree
.
├── com.foo.bar
│   ├── com
│   │   └── foo
│   │       └── bar
│   │           └── HelloWorld.java
│   └── module-info.java
└── mods
    └── com.foo.bar

6 directories, 2 files
```
Now we compile our classes to the correct destination:

```bash
~/dev/playground/helloworld_jlink$ javac -d mods/com.foo.bar/ com.foo.bar/module-info.java \
> com.foo.bar/com/foo/bar/HelloWorld.java
~/dev/playground/helloworld_jlink$ tree
.
├── com.foo.bar
│   ├── com
│   │   └── foo
│   │       └── bar
│   │           └── HelloWorld.java
│   └── module-info.java
└── mods
    └── com.foo.bar
        ├── com
        │   └── foo
        │       └── bar
        │           └── HelloWorld.class
        └── module-info.class

9 directories, 4 files
```

Let's create the JAR file. Again, by convention, the JAR file is stored in a directory called `mlib`, so we create that first, and then generate the JAR file there:

```bash
~/dev/playground/helloworld_jlink$ mkdir mlib
~/dev/playground/helloworld_jlink$ tree
.
├── com.foo.bar
│   ├── com
│   │   └── foo
│   │       └── bar
│   │           └── HelloWorld.java
│   └── module-info.java
├── mlib
└── mods
    └── com.foo.bar
        ├── com
        │   └── foo
        │       └── bar
        │           └── HelloWorld.class
        └── module-info.class

10 directories, 4 files

~/dev/playground/helloworld_jlink$ jar --create --file=mlib/com.foo.bar.hello.jar --module-version=0.0.1 --main-class=com.foo.bar.HelloWorld -C mods/com.foo.bar/ .
~/dev/playground/helloworld_jlink$ tree
.
├── com.foo.bar
│   ├── com
│   │   └── foo
│   │       └── bar
│   │           └── HelloWorld.java
│   └── module-info.java
├── mlib
│   └── com.foo.bar.hello.jar
└── mods
    └── com.foo.bar
        ├── com
        │   └── foo
        │       └── bar
        │           └── HelloWorld.class
        └── module-info.class

10 directories, 5 files
```

Excellent. We can, at this point, just test the JAR file to make sure that it's working:

````bash
~/dev/playground/helloworld_jlink$ java --module-path=mlib --module=com.foo.bar
Hola, Mundo!

~/dev/playground/helloworld_jlink$ java --module-path=mlib --describe-module com.foo.bar
com.foo.bar@0.0.1 file:///Users/z0ltan/dev/playground/helloworld_jlink/mlib/com.foo.bar.hello.jar
requires java.base mandated
contains com.foo.bar
```

Nice! We can also see that the module version that we specified while creating the "modular" JAR file has been applied correctly, and also that the dependencies for the module (basically only the core Java module, java.base, by default) is also listed correctly.

Now, all that remains to be done is to bundle the JAR file into a self-contained image:

```bash
~/dev/playground/helloworld_jlink$ jlink --module-path=$JAVA_HOME/jmods:mlib --add-modules=com.foo.bar --output=hello_app

~/dev/playground/helloworld_jlink$ ls
com.foo.bar     hello_app       mlib            mods
```

You can replace `$JAVA_HOME` with the correct path (in case this environment variable is not set correctly). Also note that I am on a mac, so I use : and the `classpath` separator. Use `;` if you're on Windows, for instance. The command is rather self-explanatory. Just note that `hello_app` is the name of the image that we wish to generate on the fly. As the ls command shows, the app has been created successfully!

```bash
~/dev/playground/helloworld_jlink$ cd hello_app/
~/dev/playground/helloworld_jlink/hello_app$ du -h .
 64K    ./bin
4.0K    ./include/darwin
192K    ./include
 72K    ./lib/jli
352K    ./lib/security
 13M    ./lib/server
 37M    ./lib
 72K    ./legal/java.base
 72K    ./legal
8.0K    ./conf/security/policy/unlimited
 12K    ./conf/security/policy/limited
 24K    ./conf/security/policy
 80K    ./conf/security
 88K    ./conf
 37M    .
```

Not bad! A 37MB image. Note that you can compress it even more by passing flags to the `jlink` command (such as `--strip-debug`, for example). If you are on macOS/Linux/Unix, you can explore more options with `man jlink`.

Finally, let's run our image!

```bash
~/dev/playground/helloworld_jlink/hello_app$ bin/java --module com.foo.bar
Hola, Mundo!
```

Nooiiiiice! Well, how do we know for sure that only the bare minimum modules have been added? Simple enough - just list the modules that this JRE contains:

```bash
~/dev/playground/helloworld_jlink/hello_app$ bin/java --list-modules
com.foo.bar@0.0.1
java.base@11.0.6
```

And that's it!


[<](2020-03-06-conditions-and-restarts-in-common-lisp.html)
[Home](/index.html)
[>](2020-04-19-hello-world-in-cobol.html)
