# JLC
An experimental compiler for the Javalette language.
It is currently incomplete;
it can not interpret or produce executables from javalette source code.

## Dependencies
* GHC with the Lens package
* BNFC
* Alex and Happy haskell libraries

## Compiling
To compile the compiler, run `make` in the src/ directory. 
This will produce the executable jlc

## Usage
The `jlc` executable is used to parse and typecheck javalette programs. It takes filename(s) as arguments or source code from stdin and outputs OK to stderr if the program was parsed and type checked without error. If an error occurs, the compiler outputs ERR to stdout followed by an error message.

## Javalette specification
The grammar we are using is the one given on the course homepage, with a 
few changes. We added an internal rule for the typed expressions, as well
as two internal types; one for functions and one for strings.

The grammar contains no reduce/reduce conflicts, and just one shift/reduce
conflict, namely the somewhat inescapable "dangling else" conflict. This
can fortunately be considered pretty harmless. Since the parser automatically
performs a shift operation every time it can do either shift or reduce, its 
behavior is well-defined:
 
    if(a) if(b) foo(); else bar();

will always be parsed to 

    if(a) {
      if(b) 
        foo(); 
      else 
        bar();
    } 

For further details, we refer to the generated Grammar.tex in the same
directory as this file.

## Implementation
There was a bit of ambiguity in the specification regarding if argument 
variables could be overloaded in the base scope of a function body, so we 
decided that it should not be allowed.
