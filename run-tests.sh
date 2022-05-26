#!/usr/bin/env -S sh -e

# Mostly copied from
# https://github.com/purcell/package-lint/blob/f16b8a131ff8443606b472aab4cefccb6ed66986/run-tests.sh

NEEDED_PACKAGES="cl-lib dash"

INIT_PACKAGE_EL="(progn \
  (require 'package) \
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
  (package-initialize) \
  (unless package-archive-contents \
     (package-refresh-contents)) \
  (dolist (pkg '(${NEEDED_PACKAGES})) \
    (unless (package-installed-p pkg) \
      (package-install pkg))))"

# Install packages
emacs -Q -batch --eval "$INIT_PACKAGE_EL"


# Byte compile
emacs -Q --batch -L . --eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile doctest.el


# Run doctests
emacs -Q --batch -L . -l doctest f doctest-batch-check-feature doctest
