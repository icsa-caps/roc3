## 1. A few words about roc3 and the motivation behind it

Roc3 is a language for specifying finite state asynchronous concurrent systems (e.g. cache coherence protocols). Roc3 is translated to murphi, a tool for model-checking such systems.

More exactly, murphi is also a language for specifying systems of this kind. Murphi source code is translated to c++ code, which is ran by a c++ compiler and does the actual model-checking.

Roc3 was designed to specify cache coherence protocols, which are a subclass of the systems describable in murphi. The reason we developed roc3 is that it is much easier and briefer to specify a cache coherence protocol in roc3. However, it turns out a much broader class of systems can be described concisely in roc3.

-----------------------------

## 2. The grammar of roc3

### 2.1 Backus-Naur Form

We specify the syntax in a variant of Backus-Naur Form (BNF):

* <> denote nonterminals;
* [] denote optional sections;
* {} denote repetition zero or more times.
* a | b denotes either a or b.
* () denote grouping.


When we use any of the above symbols in the language, we precede it by a backslash (\\).

### 2.2 Reserved words

This is the list of reserved words in the language:

* global
* networks
* ordered
* unordered
* machine
* nonsymmetric
* startstate
* boolean
* int
* set
* clear
* src
* stall
* add
* del
* contains
* count


### 2.3 Root of syntactic tree, message networks, vcs and machines

A roc3 program consists at the top level of the specification of the message networks used and the machines in the network. That is:

< Roc3 program > ::= < networks > < machines >

A message network in roc3 is a buffer (either ordered or unordered) that holds the messages sent to each instance of a machine type. Each message network is associated with some virtual channels (vcs). Machines send messages through virtual channels. Then the message ends up in the buffer corresponding to this virtual channel.

The syntax for message networks is

    < networks >     ::= EMPTY
                       | networks : < network list> ;

    < network list > ::= < network >
                       | < network List > , < network >

    < network >      ::= ordered < identifier > \{ < vcs > \}
                       | unordered < identifier > \{ vcs \}
                       | ordered \{ < vcs > \}
                       | unordered \{ < vcs > \}

The last two rules for < network > apply if there is a single message buffer in the system. In this case we don't need to supply its name, but only whether it's ordered or unordered and its virtual channels.

The syntax for virtual channels is:

    < vcs > ::= < vc >
              | < vcs > , < vc >

    < vc > ::= < identifier >

i.e. virtual channels are simply a list of identifiers.



---------------
