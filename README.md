## YOLOLISP

An s-expression based syntax for [Starbase](https://www.starbasegame.com)'s [YOLOL programming language](https://wiki.starbasegame.com/index.php/YOLOL).

Implementation is a compiler for YOLOLISP, targeting YOLOL.

Written in Emacs Lisp (...I was too lazy to set up a Common Lisp environment)

### Language Example:

YOLOLISP:

```
(set a 10)
(set :b (inc a))
(when (== :b 11)
  (set :c 1))
(// "Made in YOLOLISP!")
```

Compiles to YOLOL:
```
a = 10
:b = ++a
if :b == 11 then :c = 1 end
// Made in YOLOLISP!
```

###

Usage: currently only invokable within Emacs. Use the `yl` macro like so:

```lisp
(yl
  (set a 10)
  (set b a))
```

### TODO

 * Support full YOLOL functionality
 * Clean up the unary operator code
 * Add script minification and script size warnings
   * Constrain code to 70-length column **[DONE]**
   * Warn when script longer than 20 lines
   * Remove/strip unneccessary whitespace
 * Improve tool accessibility (...at least as far as Emacs allows!)
 * `set` macro should infer +=, -=, *=, /= **[DONE]**
 * Operator precedence - YOLOLISP over-parenthesizes output expressions
 * Constant folding in expressions
