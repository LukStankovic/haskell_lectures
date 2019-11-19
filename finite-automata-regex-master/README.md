# Finite Automata and Regular Expressions

In this project, I implemented several features for NFA, DFA, and regular expressions using Haskell and built a compiler from regular expressions to C. If you want to test the project, be sure to use the GHC as follows:

### Using GHC
  * ##### `ghci` quick start
  	* You can run the interpreter from the shell (or command line) by typing `ghci` (make sure you have it installed correctly). The interpreter provides its own prompt, allowing you to enter Haskell expressions, which are then evaluated. There is also a nice Haskell Emacs mode if you are so inclined.
    * The interpreter also provides many useful commands. These commands start with a colon, `:`. Some of particular interest are:
    	1. Load a file into the interpreter with `:load` (or `:l`)
		2. Reload the previously-loaded file with `:reload` (or `:r`)
		3. Ask the interpreter for the type of an expression with `:type` (or `:t`)
	* You can see a full list of commands with the `:?` command.

  * ##### Using GHC with the course virtual machine
  	The course virtual machine has a properly-installed version of GHC. If you use the course VM, you do not need to do anything special. As an added benefit, the course VM includes the Emacs Haskell mode.

### Working with the `fsm` program
When you type `make`, your code will be compiled to a binary named `fsm`. This binary will take a regular expression and perform one or more operations on it:
  * Dump a graphical representation of the NFA to a file using the `--nfa-dot` option.
  * Compile the NFA to C using the `--to-c` option.
  * Match the regular expression against a string using the `--match` option.
  * Run some tests using the `--test` option.

### Translating Regular Expressions to NFAs
I implemented the translation from regular expressions to NFAs in the file `RegExpToNfa.hs`. The implementation includes matching for: Empty, Epsilon, Literals, Concatenation, Alternation, and Zero or More (Star).
        
### Translating an NFA to a dot file
I implemented the conversion of an NFA into a graphical form by the toDot function in the file `NfaToDot.hs`. This function takes an NFA and outputs a description of the graphical representation of the NFA in the dot language. The dot language is a plain text graph description language. A command-line program called `dot` converts this plain text graph representation into a picture in one of many formats (pdf, svg, png, etc.). If you are curious, you can read about the dot language [here](http://www.graphviz.org/content/dot-language).

There are two most important functions: `stateToDot` and `moveToDot`.
The `stateToDot` function will generate a string in the dot language that describes a single NFA state using the dot language (it could be an empty state).
The `moveToDot` function will generate a string in the dot language that describes a single NFA transition using the dot language. This transition could be an `ε` transition.

There are two types of NFA nodes: accepting states and non-accepting states. There are also two types of edges: `ε` transitions and symbol transitions.

Here is an example session:
```bash
$ ./fsm --nfa-dot=ab.dot ab
$ cat ab.dot 
digraph fsm {
rankdir="LR";
start [shape="plaintext",label="start"];
start->0;
0[shape="circle",label="q0"];
1[shape="circle",label="q1"];
2[shape="circle",label="q2"];
3[shape="doublecircle",label="q3"];
1->2[label="ε"];
0->1[label="a"];
2->3[label="b"];
}
$ dot -Tpdf -o ab.pdf ab.dot
$
```

The following command converts the `ab.dot` file into a PDF file named `ab.pdf`:

`dot -Tpdf -o ab.pdf ab.dot`

If you run `make figs`, a number of examples will be built to test the dot translator.

### Implementing NFA Matching

The main function is `nfaMatch` function in the file `NfaMatch.hs`. The function `epsilonClosure` will be implemented to calculate the `ε` closure of the initial state when calculating the initial set of states for the NFA simulator. The `nfaMatch` function can be tested as follows:
```bash
$ ./fsm "(ab)*" --match "ab"
(ab)* matched "ab" using the naive matcher
(ab)* matched "ab" using the nfa matcher
(ab)* matched "ab" using the dfa matcher
$ ./fsm "(ab)*" --match "a" 
(ab)* did not match "a" using the naive matcher
(ab)* did not match "a" using the nfa matcher
(ab)* did not match "a" using the dfa matcher
$
```

With the `--match` argument, the `fsm` program will attempt to match the regular expression using:

  1. The naive backtracking matcher.
  2. The `nfaMatch` with the NFA built from the regular expression.
  3. The `nfaMatch` with the minimized DFA built from the NFA.

You can test the NFA simulator by running `make test`.

### Compiling a DFA to C

I implemented a regular-expression-to-C compiler. The main function is `dfaToC` in the file `DfaToC.hs`. There are two local functions used by `dfaToC`: `genCharCase` and `genTransitionCase`.

The `genCharCase` function generates a string containing the C code for handling a single character in the input (inside the `switch (*(cs++))` block). For example, in the code below, `genCharCase` is called twice, once to generate the case statement that handles `'a'`, and again to generate the case statement that handles `'b'`. The `genCharCase` function will call `genTransitionCase` for all transitions involving the character `genCharCase` was given as an argument.

The `genTransitionCase` function generates C code to handle the state transitions on a character. In the code below, it generates the inner case statements (inside the `switch (state)` blocks).

Both functions generate a string. This string contains several lines of code. The `stack` function will combine multiple lines of code as it has done elsewhere in this file.

The `--to-c` argument to `fsm` will use the `dfaToC` function to generate code to match the specified regular expression. A simple driver is included, `main.c`. It can be used as follows:
```
$ ./fsm --to-c=match.c "(ab)*" 
$ make match
gcc -o match main.c match.c
$ ./match "" ab a             
"" matched
"ab" matched
"a" did not match
$
```
This is the contents of `match.c` when compiling "(ab)*":
```
int match(const char* cs)
{
  int state = 0;
  int accept = 1;
  while (1) {
    switch (*(cs++)) {
      case 'a':
        switch (state) {
          case 0:
            state = 1;
            accept = 0;
            break;
          default: return 0;
        }
        break;
      case 'b':
        switch (state) {
          case 1:
            state = 0;
            accept = 1;
            break;
          default: return 0;
        }
        break;
      case '\0': return accept;
      default: return 0;
    }
  }
}
```
