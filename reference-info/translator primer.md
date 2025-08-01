---
title: "translator primer"
source: "https://www.gnu.org/software/hurd/hurd/documentation/translator_primer.html"
author:
published:
created: 2025-08-01
description:
tags:
  - "clippings"
---
## Small Examples on Using Translators

The [concept](https://www.gnu.org/software/hurd/hurd/concepts.html) of user-space servers, [translator](https://www.gnu.org/software/hurd/hurd/translator.html) s, is a very powerful one. Here is an introductionary text.

## Intro

The Hurd has some unique capabilities, and we created this simple image to enable you to easily try three of them:

- The simplest of translators: Hello World!
- Transparent FTP
- Mount a remote ISO file

### Hello World

To try out the simplest of translators, you can go the following simple steps:

```
$ touch hello
$ cat hello  
$ settrans hello /hurd/hello  
$ cat hello  
"Hello World!"  
$ settrans -g hello  
$ cat hello
```

What you do with these steps is first verifying that the file "hello" is empty.

Then you setup the translator /hurd/hello in the file/node hello.

After that you check the contents of the file, and the translator returns "Hello World!".

To finish it, you remove the translator from the file "hello" (and tell any active running instances to go away) via "settrans -g hello". Having done that, verify that now the file is empty again.

### Transparent FTP

We already setup a a transparent FTP translator for you at /ftp:

With it you can easily access public FTP via the file system, for example the one from the GNU project:

```
$ ls /ftp://ftp.gnu.org/
```

But you can also do this very easily yourself:

```
$ # Setup the translator on the node ftp:  
$ settrans -c ftp: /hurd/hostmux /hurd/ftpfs /
```

and you can access FTP sites via the pseudo-directory ftp:, for example with

```
$ ls ftp://ftp.gnu.org/
```

What you do here is setting up the translator /hurd/hostmux on ftp: and passing it the translator /hurd/ftpfs to use for resolving accesses as well as / as additional path component.

### ISO file mount

Now that we can access ftp.gnu.org transparently, let's mount a remote ISO file:

```
$ settrans -c mnt /hurd/iso9660fs ftp://ftp.gnu.org/old-gnu/gnu-f2/hurd-F2-main.iso
$ ls mnt/
```

It is interesting to note that since the ISO9660 format is indexed, ftpfs does not have to download the whole ISO file, it merely fetches what iso9660fs requests.

These were only three basic usages of translators on the Hurd. We're sure you'll quickly see many other ways to use this.

As a last comment: You can setup a translator on any node you have access to, so you can for example mount any filesystems as normal user.

You might currently be logged in as root, but you could just as well do the same as normal user.

Why don't you try it out?