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

;; We need Org for output formatting.

(require 'org)

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

(defvar ldoc-nextform-types '("function" "generic function" "macro" "method"
			      "substitution" "compiler-macro"
			      "class" "prototype" "method"))

(defun ldoc-nextform-string (definition)
  (let ((nextform (ldoc-definition-nextform definition)))
    (if (member (ldoc-definition-type definition)
		ldoc-nextform-types)
	(if (null nextform)
	    "()"
	    (prin1-to-string nextform)))))

(defun ldoc-format-type (type)
  (let ((pos (position-if (lambda (x)
			    (string= type x))
			  ldoc-def-types)))
    (when (numberp pos)
	  (nth pos ldoc-def-names))))

(defvar ldoc-blank-line-regexp "^[[:space:]]*$")


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

;;; Producing an org buffer from a lisp file's documentation.

(defun ldoc-all-definitions ()
  (save-excursion
    (goto-char (point-min))
    (let (def defs)
      (while (setf def (ldoc-next-definition))
	(push def defs))
      defs)))

(defun* ldoc-make-org-buffer (&optional (output-buffer-name "*ldoc-org-buffer*")
					(input-buffer (current-buffer)))
  (interactive)
  (let ((output-buffer (get-buffer-create output-buffer-name))
	(defs (ldoc-all-definitions)))
    (prog1 output-buffer
      ;; sort definitions
      (setf defs (sort defs #'(lambda (d1 d2)
				(string< (ldoc-definition-name d1)
					 (ldoc-definition-name d2)))))
      ;; print definitions
      (with-current-buffer output-buffer
	(delete-region (point-min) (point-max))
	(dolist (def defs)
	  (insert (format "* %s /%s/\n\n%s\n\n"
			  (ldoc-definition-name def)
			  (ldoc-nextform-string def)
			  (ldoc-definition-docstring def))))))))

;; TODO do not include long nextforms

;;; Finding files to scan.

(defvar ldoc-files '())

(defvar ldoc-output-directory nil)

(defun* ldoc-make-org-file (&optional (file (buffer-file-name (current-buffer))))
  (interactive)
  (let ((output-file (expand-file-name 
		      (concat (file-name-nondirectory (file-name-sans-extension file))
			      ".org")
		      (file-name-as-directory ldoc-output-directory))))
    (with-temp-buffer
      (insert-file-contents-literally file)
      (goto-char (point-min))
      (with-current-buffer (ldoc-make-org-buffer)
	(write-file output-file)))))
  
(let ((ldoc-output-directory "~/rlx/doc"))
  (ldoc-make-org-file))

;;; Producing one big indexed HTML doc file for a group.





(provide 'ldoc)
;;; ldoc.el ends here
