;;; doctest.el --- Run tests in Elisp docstring  -*- lexical-binding: t; -*-

;; Copyright (C) 2020, 2022  Xu Chunyang, Hraban Luyat

;; Author: Xu Chunyang, Hraban Luyat
;; Homepage: https://github.com/hraban/doctest.el
;; Package-Requires: ((emacs "25"))
;; Keywords: lisp, tools
;; Version: 0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Run tests in Elisp docstring

;;; Code:

;;;;
;;;; PARSING TESTS FROM DOCSTRINGS
;;;;

(require 'cl-lib)
(require 'dash)
(require 'loadhist)

(defvar doctest--test-start (rx bol (* " ") "("))

(defun doctest--looking-at (rex)
  "Like ‘looking-at’ but move to end of match, if found."
  (when (looking-at rex)
    (goto-char (match-end 0))))

(defun doctest--trim-newline (s)
  "Remove trailing newline from S, if any"
  (replace-regexp-in-string "\n$" "" s))

(defun doctest--read-msg-fixture (&optional result)
  "Get the message fixture in this buffer from point, if any.

If the buffer, at point, looks like this:

-> foo
-> bar

This function returns \"foo\nbar\"

(with-temp-buffer
  (insert \"-> foo\n-> bar\")
  (goto-char (point-min))
  (doctest--read-msg-fixture))
=> \"foo\nbar\"

(with-temp-buffer
  (doctest--read-msg-fixture))
=> nil

It also advances the pointer to the first line that isn’t a match (so this is a
NOP if there is no match).
"
  (if (doctest--looking-at (rx (* " ") "-> "))
      (let ((old (point)))
        (forward-line)
        (doctest--read-msg-fixture (cons (buffer-substring-no-properties old (point)) result)))
    (-some->> result
              reverse
              (apply #'concat)
              doctest--trim-newline)))

(defun doctest--read-line ()
  "Read a sexp from the point in buffer and move to the line following it.

If the pointer is not on a valid sexp, move forward one line and return NIL."
  (let ((sexp (condition-case e (read (current-buffer))
                (ignore e)
                (invalid-read-syntax nil))))
    (forward-line)
    sexp))

(defun doctest--next-doctest ()
  "Find the next doctest in this buffer.

Intended to be used on a temp buffer whose entire contents are set to the
docstring. Returns NIL if no doctest is found."
  (when (search-forward-regexp doctest--test-start nil t)
    ;; The rest actual reader loop re-uses the regex which is anchored to the
    ;; start-of-line, so reset the point to help it out.
    (beginning-of-line)
    (let ((code (doctest--collect-while-true (lambda ()
                                               (when (doctest--looking-at doctest--test-start)
                                                 (backward-char)
                                                 (doctest--read-line))))))
      (if (-> code length (= 0))
          ;; This wasn’t a valid sexp. Keep looking.
          (doctest--next-doctest)
        (let ((msg (doctest--read-msg-fixture)))
          (if (doctest--looking-at (rx (* " ") "=> "))
              (list (pcase code
                      ;; Unpack if it’s just one sexp
                      (`(,sexp) sexp)
                      ;; Wrap it in a progn if it’s multi line
                      (_ `(progn ,@code)))
                    (doctest--read-line) msg)
            ;; This didn’t have a result fixture, so it’s not a doctest. Keep
            ;; looking.
            (doctest--next-doctest)))))))

(defun doctest--collect-while-true (f)
  "Call function F and collect its results while it returns a non-NIL value"
  (cl-loop for x = (funcall f) while x collect x))

(defun doctest--docstring-tests (docstring)
  "Return tests in this DOCSTRING."
  (with-temp-buffer
    (insert docstring)
    (goto-char (point-min))
    (save-excursion
      (save-match-data
        (doctest--collect-while-true 'doctest--next-doctest)))))

(defun doctest--function-tests (function)
  "Return tests in FUNCTION's docstring."
  (-some-> (documentation function 'raw) doctest--docstring-tests))

;;;;
;;;; RUNNING TESTS
;;;;

(cl-defmacro doctest--with-around ((f advice) &body body)
  "Advise function F with ADVICE (around) only in the scope of this BODY"
  (declare (indent 1))
  `(progn
     (advice-add ,f :around ,advice)
     (unwind-protect
         (progn ,@body)
       (advice-remove ,f ,advice))))

(defun doctest--capture (f &rest args)
  "Run f and capture its ‘message’ output, returning (val output).

(doctest--capture (lambda () (message \"foo%s\" \"bar\") (message \"quu\") 3))
=> (3 \"foobar\nquu\")

(doctest--capture (lambda ()))
=> (nil nil)

(doctest--capture 'identity 4)
=> (4 nil)

"
  (with-temp-buffer
    (let (wrote-something)
      (doctest--with-around ('message (lambda (orig &rest args)
                                        (ignore orig)
                                        (setf wrote-something t)
                                        (unless (= (point) (point-min))
                                          (insert "\n"))
                                        (let ((text (apply #'format-message args)))
                                          (insert text)
                                          ;; Message returns the inserted string
                                          text)))
        (list (apply f args) (when wrote-something (buffer-string)))))))

;;;;
;;;; DOCTESTS -> ERT TESTS
;;;;

(defmacro doctest--ert-func-helper (fsym)
  (interactive "a")
  `(ert-deftest ,fsym ()
     :tags '(doctest)
     ,(format "Doctests for ‘%s’" fsym)
     (dolist (test (doctest--function-tests ',fsym))
       (pcase-let* ((`(,expr ,want-val ,want-msg) test)
                    (`(,got-val ,got-msg) (doctest--capture (lambda () (eval expr 'lexical)))))
         (should (equal want-val got-val))
         (should (equal want-msg got-msg))))))

(defun doctest--ert-func (fsym)
  "Transform all tests in this feature to ERT tests.

We’re in a function, and we want to evaluate a macro with runtime
parameters. Fundamentally we need to call eval because otherwise
the macro would be expanded at runtime. I don’t want to expand at
runtime because I want to allow users to call this function
interactively, after loading their file and their functions. On
the other hand, I can’t turn everything into a function down the
call stack, because at some point we use ert-deftest, which is a
macro."
  (eval `(doctest--ert-func-helper ,fsym)))

(defun doctest--feature-functions (feature)
  "Get all top-level defun symbols defined in FEATURE.

Includes macros.
"
  (->> feature
       feature-symbols
       (--map (pcase it
                (`(defun . ,func) func)))
       (cl-remove nil)))

(defun doctest-check-function (fsym)
  "Doctest a single function.

Runs the tests in ERT.
"
  (interactive "a")
  (doctest--ert-func fsym)
  (ert fsym))

(defun doctest-register-feature (feature)
  "Register all doctests in FEATURE as ert tests, without running.

Doesn’t run the actual tests. Use this if you want to control
running of the ert tests yourself.

Returns a list of all function names, which also are the test names.
"
  (interactive (list (read-feature "Check feature: ")))
  (->> feature
       doctest--feature-functions
       (cl-mapc 'doctest--ert-func)))

(cl-defun doctest-check-feature (feature)
  "Check doctests of all functions defined in FEATURE using ert.

Both registers and runs all tests in this feature.

Uses the TEST function to actually run the tests, which is (ert
...) by default.
"
  (interactive (list (read-feature "Check feature: ")))
  (--> (doctest-register-feature feature)
       (ert `(member ,@it))))

(defun doctest-batch-check-feature ()
  "Helper function to run all doctests for a feature non-interactively.

Usage:

  emacs --batch -l ert -l doctest.el doctest-batch-check-feature feat-1 feat-2

You will need to load dash somehow, probably using ‘package-initialize’.

If you want to roll your doctests into a larger test suite and run them all,
non-interactively, at the same time, use ‘doctest-register-feature’. For
example:

  emacs --batch \\
        ... \\
        --eval \"(doctest-register-feature 'my-feature)\" \\
        ... \\
        -f ert-run-tests-batch-and-exit
"
  (--> command-line-args-left
       (mapcar 'intern it)
       (mapcan 'doctest-register-feature it)
       (ert-run-tests-batch-and-exit `(member ,@it))))

(provide 'doctest)

;;; doctest.el ends here
