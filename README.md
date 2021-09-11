## YOLOLISP

An s-expression based syntax for [Starbase](https://www.starbasegame.com)'s [YOLOL programming language](https://wiki.starbasegame.com/index.php/YOLOL).

Implementation is a compiler for YOLOLISP, targeting YOLOL.

Written in Emacs Lisp (...I was too lazy to set up a Common Lisp environment)

### Language Example:

YOLOLISP:

```elisp
(yl
 ;; Declarations are optional, but useful for hinting the optimizer.
 ;; In this case, the if-statement later can be made branchless.
 (declare (type integer :c e))

 ;; Assignment
 (set a  10
      :b (inc a))

 ;; Simple WHEN conditional
 (when (== :b 11)
   (set :c 1))

 ;; IF conditional (optimised into a branchless form)
 (if :c
     (assign d 1)
   (assign d 2))

 ;; Arithmetic expressions
 (set e (* d (+ :c :b)))
 (set f (* a (* a (+ a a))))

 ;; Literal output for when YOLOLISP isn't enough
 (literal "\nz = e + f")

 ;; Lisp comment vs. output YOLOL comment
 (// "Made in YOLOLISP!"))
```

Compiles to YOLOL:
```
a=10 :b=a++ if :b==11 then :c=1 end d=2*0^:c+1*:c e=d*(:c+:b)
f=a*a*(a+a)
z = e + f //Made in YOLOLISP!
```

### Usage:

Currently only invokable within Emacs. Use the `yl` macro like so:

```lisp
(yl
  (set a 10)
  (set b a))
```

### Goals

 * Support full YOLOL functionality across different chip types
 * Create a reasonably good optimizing and minifying compiler
 * Obtain a good level of documentation and usability

### TODO

 * Add script minification and script size warnings
   * Constrain code to 70-length column **[DONE]**
   * Warn when script longer than 20 lines
   * Remove/strip unneccessary whitespace
     * In assignments **[DONE]**
     * After function calls
     * Within `if` conditionals
   * Output operator precedence - YOLOLISP over-parenthesizes output expressions
     * Basic precedence for arithmetic operations **[DONE]**
     * Advanced precedence of all YOLOL operators (requires cleanup)

 * DECLARE form: specify YOLOL chip, types, optimizations, etc.
   * Declaration environment setup **[DONE]**
   * Type declarations **[DONE]**
   * Optimization declarations
   * Chip/function/column constraints declarations

 * Optimization
   * Optimizer framework (as a precompilation stage!) **[DONE]**
   * Low hanging fruit optimizations:
     * Basic constant folding within expressions
     * Branchless optimizations for conditionals *[IN PROGRESS]*

 * Compiler usability and accessibility
   * Replace ELPA/MELPA dependencies with built-in Emacs stuff **[DONE]**
   * Create a compiler entrypoint shell script
   * Useful compilation error messages

 * Misc. features:
   * `set` macro should infer +=,-=,*=,/= **[DONE]**
   * More useful macros and utilities
   * Interface with some YOLOL emulators
   * An Emacs YOLOLISP environment?
   * Error-triggering for code speedup -- error handling system?
   * Looping helper macros?
   * Smarter type inferencing?!
   * Generic output literal for greater control **[DONE]**

 * Documentation, cleanup, maintenance
   * Clean up the unary operator code...
   * Actually add documentation!
   * Function docstrings
   * ELISP code style compliance
   * Split into multiple files
