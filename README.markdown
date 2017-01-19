Roc3
---

What is it?
---
Roc3 is a language for specifying finite state asynchronous concurrent systems
(e.g. cache coherence protocols). The roc3 source code is translated to
equivalent murphi (version 5.4.9) source code.


What is the advantage of roc3 over murphi?
---

Roc3 is much more consice and simpler than murphi. For example, specifying a
cache coherence protocol in roc3 can be a quarter of an equivalent murphi file
(see the examples). It is also much more natural language for describing
networks of finite state machines.


What you need
-------

You need Haskell, the programming language the translator is written in. You
can download it from here:

`https://www.haskell.org/`


We suggest you also have the vim text editor in case you want to edit or
inspect the murphi source code, as it is the only text editor with syntax
highlighting for murphi. You can get the add-on for murphi here:

`https://github.com/melver/murphi.vim`


You can download the murphi model-checker and have a complete model-checking
tool from here:

 `http://mclab.di.uniroma1.it/site/index.php/software/18-murphi`



How to use it
------
We provide details on how to run the application on a unix based environment.

First you need to download the repository. At a terminal window, navigate to the directory where you have placed the downloaded code. Type the command ``` cabal build```. This should build the executable.

The executable is in `dist/build/roc3` and should not be moved to another folder. To run the application type

`roc3 < name of roc3 file > < name of target file > `

This will produce a compilable murphi source file. To actually do the model checking, consult the murphi documentation.

Note: you can omit the extention ".m" when specifying the name of the target file.

The roc3 language
-----
See the wiki page for the language specification.

Code structure
------
See the file code_stucture.

License
-----


Authors
----
