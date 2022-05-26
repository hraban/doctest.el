;;; doctest.el --- Run tests in Elisp docstring  -*- lexical-binding: t; -*-

;; Copyright (C) 2020, 2022  Xu Chunyang, Hraban Luyat

;; Author: Xu Chunyang
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

(require 'cl-lib)
(require 'loadhist)

;; NOT used in code, for testing
(defun doctest--length (lst)
  "Return the length of list LST.

(doctest--length '())
=> 0

(doctest--length '(a))
=> 1

(doctest--length '(a b))
=> 2
"
  (length lst))

(defun doctest--eval (expr)
  (condition-case e
      (list nil (eval (read-from-string expr) 'lexical))
    (error (list e nil))))

(defvar doctest--rex
  (let ((sexp-line (rx bol "(" (* not-newline) ")" eol)))
    (rx (group-n 1 (regexp sexp-line)
                   (* "\n" (regexp sexp-line)))
        (? "\n" (* " ") "->" (* " ") (group-n 2 (* not-newline)))
        "\n" (* " ") "=>" (* " ") (group-n 3 (* not-newline))))
  "Regular expression to match a doctest, including potential message output")

(defun doctest--next-doctest ()
  (when (search-forward-regexp doctest--rex nil t)
    (mapcar (lambda (group)
              ((match-string-no-properties '(1 2 3))))

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
  ;; this is dash’s -some-> in a nutshell
  (let ((docstr (documentation function 'raw)))
    (when docstr
      (doctest--docstring-tests docstr))))

(defun doctest--feature-functions (feature)
  (let (funcs)
    (dolist (entry (feature-symbols feature))
      (pcase entry
        (`(defun . ,func)
         (push func funcs))))
    (nreverse funcs)))

(defun doctest--feature-tests (feature)
  (cl-mapcan #'doctest--function-tests
             (doctest--feature-functions feature)))

(defun doctest-check-feature (feature)
  "Check functions defined in FEATURE."
  (interactive (list (read-feature "Check feature: ")))
  (let* ((tests (doctest--feature-tests feature))
         (total (length tests))
         passed)
    (cl-loop for test in tests
             for i from 1
             for progress = (format "[%d/%d]" i total)
             do
             (cl-destructuring-bind (expr msg want) test
               (pcase (doctest--eval expr)
                 (`(nil ,got)
                  (cond
                   ((equal got want)
                    (push test passed)
                    (message "%s %s pass" progress (car expr)))
                   (t
                    (let ((print-quoted t))
                      (message "%s %s got: %s want: %s" progress expr got want)))))
                 (`(,err ,_)
                  (let ((print-quoted t))
                    (message "%s %s got error: %s"
                             progress
                             expr (error-message-string err)))))))
    (message "Total: %d, Pass: %d, Fail: %d"
             total (length passed) (- total (length passed)))
    (list    total (length passed) (- total (length passed)))))

;; emacs -Q --batch -L . -l doctest -f doctest-batch-check-feature doctest
(defun doctest-batch-check-feature ()
  (unless noninteractive
    (user-error "This function is only for use in batch mode"))
  (let ((total 0)
        (passed 0)
        (failed 0)
        (features 0))
    (dolist (feature (delete-dups (mapcar #'intern command-line-args-left)))
      (cl-incf features)
      (pcase-exhaustive (doctest-check-feature feature)
        (`(,total_ ,passed_ ,failed_)
         (cl-incf total  total_)
         (cl-incf passed passed_)
         (cl-incf failed failed_)))
      (message nil))
    (if (zerop failed)
        (message "Test passed")
      (message "Test failed"))
    (kill-emacs (if (zerop failed) 0 1))))

(provide 'doctest)
;;; doctest.el ends here
