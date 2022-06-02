# doctest - Run tests in Elisp docstring

## Example

For example, with this Emacs Lisp file, `M-x doctest-check-feature foo` will check if `(foo-add 1 2)` equals to `3`.

```emacs-lisp
;;; foo.el --- Foo  -*- lexical-binding: t; -*-

(defun foo-add (x y)
  "Add X to Y.

  (foo-add 1 2)
  => 3"
  (+ x y))

(provide 'foo)
```

You can also test for message output:

```emacs-lisp
;;; foo.el --- Foo  -*- lexical-binding: t; -*-

(defun foo-say (temp)
  "Talk about the weather

  (foo-say 'cold)
  >> It’s cold outside
  => brr

  (foo-say 'hot)
  >> It’s hot today
  >> ... very hot!
  => pfew
  "
  (pcase temp
    ('cold (message "It's cold outside") 'brr)
    ('hot (message "It's hot today") (message "... very hot!") 'pfew)))

(provide 'foo)
```

Note the apostrophe (`’`) versus quote (`'`): that’s intentional. Emacs’
`message` function transforms quotes to apostrophes.

## Syntax

Use this format:

```
(my-func arg1 arg2 ...)
>> message output (optional)
=> RESULT
```

All functions must exist in a file that provides a feature.

## Running Tests

### Interactively

use `M-x doctest-check-feature FEATURE` to check functions defined in `FEATURE`.

### In Batch Mode

Use `doctest-batch-check-feature`, e.g., to check features `foo` and `bar`

``` shell
emacs -Q --batch -L /path/to/doctest -l doctest -f doctest-batch-check-feature foo bar
```
