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

;; Some mock functions to demonstrate how this works

(defun doctest--test1 (lst)
  "Return the length of list LST.

(doctest--test1 '())
=> 0

(doctest--test1 '(a))
=> 1

(doctest--test1 '(a b))
=> 2
"
  (length lst))

(defun doctest--test2 (temp)
  "Talk about the weather

  (doctest--test2 'cold)
  -> It’s cold outside
  => brr

  (doctest--test2 'hot)
  -> It’s hot today
  -> ... very hot!
  => pfew
  "
  (pcase temp
    ('cold (message "It's cold outside") 'brr)
    ('hot (message "It's hot today") (message "... very hot!") 'pfew)))

(defun doctest--test3 (x y)
  "Add two numbers.

  (setf a 1)
  (setf b 2)
  (doctest--test3 a b)
  => 3
"
  (+ x y))

;; Actual code below

(require 'cl-lib)
(require 'dash)
(require 'loadhist)

(defvar doctest--test-start (rx bol (* " ") "("))

(defun doctest--eval (expr)
  (condition-case e
      (cons nil (doctest--capture (lambda () (eval expr 'lexical))))
    (error (list e nil nil))))

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

(defun doctest--check-doctest (test message)
  (pcase-let* ((print-quoted t)
               (`(,expr ,want-val ,want-msg) test)
               (`(,err ,got-val ,got-msg) (doctest--eval expr)))
    (cond
     (err
      (funcall message "FAIL %s: got error: %s" expr (error-message-string err))
      nil)
     ((not (equal got-val want-val))
      (funcall message "FAIL %s: return value mis-match: got: %S want: %S" expr got-val want-val)
      nil)
     ((not (equal got-msg want-msg))
      (funcall message "FAIL %s: message output mis-match: got: %S want: %S" expr got-msg want-msg)
      nil)
     (t
      (funcall message "pass %s" (if (consp expr) (car expr) expr))
      t))))

(defun doctest--feature-functions (feature)
  (->> feature
       feature-symbols
       (--map (pcase it
                (`(defun . ,func) func)))
       (cl-remove nil)))

(defun doctest--feature-tests (feature)
  (cl-mapcan 'doctest--function-tests (doctest--feature-functions feature)))

(defun doctest-check-function (f)
  "Doctest a single function.

Returns nil if there are failing tests, non-nil otherwise. Notably, if there are
no tests, this returns non-nil.
"
  (interactive "a")
  (--every-p (doctest--check-doctest it #'message) (doctest--function-tests f)))

(defun doctest--check-multiple (tests)
  (let* ((total (length tests))
         (passed (cl-loop for test in tests
                          for i from 1
                          for msg = (lambda (fmt &rest args)
                                      (apply #'message (concat "[%d/%d] " fmt) i total args))
                          count (doctest--check-doctest test msg)))
         (failed (- total passed)))
    (message "Total: %d, Pass: %d, Fail: %d"
             total passed failed)
    (list    total passed failed)))

(defun doctest-check-feature (feature)
  "Check functions defined in FEATURE."
  (interactive (list (read-feature "Check feature: ")))
  (doctest--check-multiple (doctest--feature-tests feature)))

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
