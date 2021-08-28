## YOLOLISP

An s-expression based syntax for [Starbase](https://www.starbasegame.com)'s [YOLOL programming language](https://wiki.starbasegame.com/index.php/YOLOL).

Implementation is an compiler for YOLOLISP, targeting YOLOL.

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
* Clean up the unary operator
* Add script minification and script size warnings
*  ( as Emacs allows :) )
