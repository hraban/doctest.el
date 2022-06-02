;;; doctest-test.el --- Test doctest.el   -*- lexical-binding: t; -*-

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

;;; Code:

(defun doctest-test--1 (lst)
  "Return the length of list LST.

(doctest-test--1 '())
=> 0

(doctest-test--1 '(a))
=> 1

(doctest-test--1 '(a b))
=> 2
"
  (length lst))

(defun doctest-test--2 (temp)
  "Talk about the weather

  (doctest-test--2 'cold)
  >> It’s cold outside
  => brr

  (doctest-test--2 'hot)
  >> It’s hot today
  >> ... very hot!
  => pfew
  "
  (pcase temp
    ('cold
     (message "It's cold outside")
     'brr)
    ('hot
     (message "It's hot today")
     (message "... very %s!" temp)
     'pfew)))

(defun doctest-test--3 (x y)
  "Add two numbers.

  (setf a 1)
  (setf b 2)
  (doctest-test--3 a b)
  => 3
"
  (+ x y))

(provide 'doctest-test)

;;; doctest-test.el ends here
