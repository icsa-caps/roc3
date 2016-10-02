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


### 2.3 Root of syntactic tree, message networks and VCs

A roc3 program consists at the top level of the specification of the message networks used and the machines in the network. That is:

< Roc3 program > ::= < networks > < machines >

A message network in roc3 is a buffer (either ordered or unordered) that holds the messages sent to each instance of a machine type. Each message network is associated with some virtual channels (VCs). Machines send messages through virtual channels. Then the message ends up in the buffer corresponding to this virtual channel.

The syntax for message networks is

    < networks >     ::= EMPTY
                       | "networks :" < network list> ";"

    < network list > ::= < network >
                       | < network List > , < network >

    < network >      ::= "ordered" [ < identifier > ] \{ < VCs > \}
                       | "unordered" [ < identifier > ] \{ VCs \}

We don't include an identifier for the network if there is a single message buffer in the system. In this case need only say whether it's ordered or unordered and its virtual channels.

The syntax for virtual channels is:

    < VCs > ::= < VC >
              | < VCs > , < VC >

    < VC > ::= < identifier >

i.e. virtual channels are simply a non-empty list of identifiers.



---------------


### 2.4 Machines

Revisiting the top rule for a roc3 program

    < roc3 program > ::= < networks > < machines >

We see we must specify the machines of the system after the the message buffers. The syntax for specifying machines is the following:

    < machines > ::= < machine >
                   | < machines > < machine >

    < machine > ::= < symmetric mac >
                  | < non-symmetric mac >


As you can see, we split machines into two big categories : symmetric and non-symmetric ones. When specifying a machine in roc3, we are essentially specifying a machine type (e.g. cache or directory). A machine type in the system we describe may have several instances; all machines of a given type are represented at the bottom level as an array, with size equal to the number of instances of the machine.

As you can see, in roc3 machines can fall into one of two categories: symmetric and non-symmetric ones.
A machine is symmetric if we don't refer to explicit instances of the machine. When refering to a particular instance, we do so by a variable. We never give away an explicit index of the array for this machine type. An example is caches in a cache coherence protocol.
On the other hand, a machine should be declared non-symmetric when we want to refer explicitly to instances of this type, not only through variables. For example, in the MSI and the MI protocols, the directory is a non-symmetric machine in roc3, as we send messages to the single instance of the directory explicitly and not only by a variable (e.g. source of the received message).
We will give examples of symmetric and non-symmetric machines later, when we describe the syntax for messages and how machine send messages. For now, we will progress with the syntax for machines:

    < symmetric mac >    ::= "machine" < identifier > < range >
                             \{ < machine body > \}

    < non-symmetric mac> ::=  "machine" < identifier >
                             \{ < machine body > \}
                           | "nonsymmetric machine" < identifier > < range >
                             \{ < machine body > \}

    < range >            ::= \[ < number > \]

    < machine body >     ::= < startstate >
                             < fields>
                             <guard-responses >


When we don't include the range of a non-symmetric machine nor the keyword "nonsymmetric", it is parsed as a non-symmetric machine with a single instance. This is handy when declaring e.g. the directory in a cache coherence protocol.

The range gives the number of instances of the machine. In the machine body we specify the functionality of the machine.


-------------------------

### 2.5 Starting state and fields

#### 2.5.1 Machine startstate

Each machine type is a finite automaton and has some states. The syntax for the starting state is

    < startstate > ::= "startstate :" < identifier > ";"


#### 2.5.2 Fields

Each machine type may need some variables to hold information that affects the behaviour of a machine, along with the current state. For example, in the MSI protocol the directory must have a list of the sharers of a cache block or hold the modifier. The syntax for fields is

    < fields > ::= EMPTY
                 | < field list > "," < field >
                 | < field >

    < field > ::= < type declaration > < starting value >

    < starting value > ::= EMPTY
                         | "( < identifier > )"
                         | "( < number > )"


The language supports the following types for machine fields: booleans, integers in a given range, enums, any machine type defined in the system and arrays and sets containing a single type of these. The syntax is

* for booleans:  

  ` "boolean" < identifier> `

* for bounded integers:

  `"int" \[ < num > ".." < num > \] < identifier >`

  where the first < num > is the lowest value and the second < num > the largest value the variable can take

* for enums:

  ` < enum name > \{ < value list > \} `

  where < enum name > is an < identifier > and < value list > is a comma-separated list of < identifier >
* for fields of a machine type:

  `< machine type> < identifier >`

  where the machine type is an `< identifier >`

* for arrays there are two possibilities:

  - they can be indexed by numbers, like arrays in most programming languages:

    `\[ < array size > \] < type declaration >`

  - or they can be indexed by a machine type i.e. they are maps:

    `\[ < machine type > \] < type declaration >`

* sets can also be indexed by either integers or machine types.The syntax is

  - `"set" \[ < size > \] < type declaration >`

  - `"set" \[ < machine type > \] < type declaration >`

A few remarks on sets and arrays: we index a set or an array by a machine type if we want one entry for each instance of that machine type. Secondly, since the sets are of bounded size, their only difference is that they are unordered. Lastly, both are defined recuresively.

Here are all the syntactic rules for type declarations:

    < type declaration > ::= "boolean" < identifier >
                           | "int" \[ < num > ".." < num > \] < identifier >
                           | < enum name > \{ < value list > \}
                           | < machine type > < identifier >
                           | \[ < array size > \] < type declaration >
                           | "set" < range > < type declaration >
                           | "set" \[ < machine type > \] < type declaration >



------------------------

### 2.6 Guards





---------------------

### 2.7 Responses



---------------------

### 2.8 Messages




---------------------










---------------------
