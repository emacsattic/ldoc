;;; ldoc.el --- extract and format documentation from lisp files


;; Copyright (C) 2009  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: lisp, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; LDOC reads Common Lisp and Emacs Lisp files, extracting headers,
;; definitions, and docstrings. It works with org-mode to produce
;; documentation files and indices in HTML and other formats. 

;;; Code:

;;; Prerequisites

;; We require radio-mode (another of my projects) for defining groups
;; of files to process and index.

;; http://github.com/dto/radio

(require 'radio) 

;; We also need Org for output formatting.

(require 'org)

;;; Test forms

(defvar *ldoc-test* 
  (make-radio-group :name "rlx"
		   :base-directory "~/rlx/"
		   :include "*\\.lisp$"))

;;; Definitions

(defstruct ldoc-definition name type nextform docstring source-code file)

;;; Scanning definitions 

;; A definition line is defined by the regexp `ldoc-def-regexp' below. 

(defvar ldoc-def-types '("defadvice" "defalias" "defgeneric"
			   "defmacro" "defmethod" "defun"
			   "defsetf" "defsubst" "defcondition"
			   "define-derived-mode" "define-minor-mode"
			   "define-method-combination"
			   "define-setf-expander" "define-compiler-macro"
			   "define-symbol-macro" "define-modify-macro"
			   "defconstant" "defvar" "defparameter"
			   "defgroup" "defcustom" "defface"
			   "defclass" "defstruct" "defpackage"
			   "deftype"
			 ;; for CLON
			 "define-prototype" "define-method"
			 "defcell"))
					
;; these two lists' members should be kept in correspondence

(defvar ldoc-def-names '("advice" "alias" "generic function"
			   "macro" "method" "function"
			   "setf function" "substitution" "condition"
			   "derived mode" "minor mode"
			   "method combination"
			   "setf expander" "compiler macro"
			   "symbol macro" "modify macro"
			   "constant" "variable" "parameter"
			   "customization group" "customization variable"
			   "face" "class" "structure" "package" "type"
			 ;; for CLON
			 "prototype" "method" "cell"))

(defvar ldoc-def-regexp (concat "(" 
				;; definition type 
				"\\(" 
				(regexp-opt ldoc-def-types)
				"\\)"
				;; name being defined
				" \\([^\n ]*\\)"
				;; scan to beginning of next form
				"\\([[:space:]]+\\)"))

(defun ldoc-format-type (type)
  (let ((pos (position-if (lambda (x)
			    (string= type x))
			  ldoc-def-types)))
    (when (numberp pos)
	  (nth pos ldoc-def-names))))

(defvar ldoc-blank-line-regexp "^[[:space:]]*$")

;; (ldoc-next-definition)

(defvar ldoc-doc-begin-regexp "^\\([[:space:]]*\\)\\(\"\\)")

(defun ldoc-next-blank-line-position ()
  (save-excursion
    (when (re-search-forward ldoc-blank-line-regexp nil t)
      (point-at-bol))))

(defun ldoc-next-docstring ()
  "Find the next docstring (if any) with the following procedure:

  - Scan lines for any definition line
  - Find and parse definition
  - Scan following lines until blank line 
    (i.e. empty or consisting solely of whitespace)
    looking for docstring
  - If found, use elisp #'read to snarf string

This means functions should be separated by at least a blank
line, and docstrings should always start on a fresh line."
  (when (re-search-forward ldoc-doc-begin-regexp
			   (or (ldoc-next-blank-line-position)
			       (point-max))
			   t)
    ;; jump to opening double-quote of docstring
    (goto-char (match-beginning 2))
    (read (current-buffer))))

(defun ldoc-next-definition ()
  (when (re-search-forward ldoc-def-regexp nil t)
    (make-ldoc-definition :type (ldoc-format-type (match-string-no-properties 1))
			  :name (match-string-no-properties 2)
			  :nextform (save-excursion 
				      (goto-char (match-beginning 3))
				      (read (current-buffer)))
			  :docstring (ldoc-next-docstring)
			  :file (buffer-file-name (current-buffer)))))


    
;;; Producing one big indexed HTML doc file for a group.





(provide 'ldoc)
;;; ldoc.el ends here
