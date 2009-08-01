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

(require 'cl)

;;; Defining groups of files

;; A group is a set of files that are handled together.

(defstruct ldoc-group 
  ;; Required slots.
  name ;; String name of group. 
  base-directory ;; Directory to scan for files.
  format ;; A keyword like :lisp, :text, :org
  include ;; Regexp or list of files to include
  ;; Optional slots
  description ;; Optional string description.
  exclude  ;; Regexp or list of files to exclude
  etags-file  ;; etags filename (relative to base-directory).
              ;; The default is TAGS.
  etags-arguments ;; List of argument strings for etags.
  ;; Other slots
  selected-p ;; When non-nil, multi-group operations apply to this group. 
  files ;; Cached list of files in this group; updated on each file scan.
        ;; See also `ldoc-scan-group-files'.
  )

(defun ldoc-match-regexp-or-list (filename regexp-or-list)
  "Return non-nil if FILENAME is either a member of the list (or
matches the regexp) REGEXP-OR-LIST."
  (if (null regexp-or-list)
      nil
      (etypecase regexp-or-list
	(string (string-match regexp-or-list filename))
	(list (member filename regexp-or-list)))))

(defun ldoc-filter-files (files regexp-or-list)
  "Return FILES with any matching files removed.  If the second
argument is a regular expression, remove files matching the
regexp. If it's a list, remove files matching any filename in the
list. See also `ldoc-match-regexp-or-list'."
  (labels ((match (filename)
	     (ldoc-match-regexp-or-list filename regexp-or-list)))
    (remove-if #'match files)))

(defun ldoc-get-group-files (group)
  "Obtain a list of all the available files in the group GROUP."
  (let ((dir (ldoc-group-base-directory group)))
    (labels ((expand (filename)
	       (expand-file-name filename dir)))
      (let* ((include (ldoc-group-include group))
	     (files (etypecase include
		      (string (directory-files dir nil include))
		      (list (mapcar #'expand include)))))
	(mapcar #'expand 
		(remove-duplicates 
		 (ldoc-filter-files files (ldoc-group-exclude group))
		 :test 'equal))))))

(defun ldoc-load-group-files (group)
  (setf (ldoc-group-files group)
	(ldoc-get-group-files group)))

(defun ldoc-create-group (&rest forms)
  (let ((group (apply #'make-ldoc-group forms)))
    (prog1 group 
      (ldoc-load-group-files group))))

;;; Groups table

(defvar *ldoc-groups* nil)

(defun ldoc-init-groups ()
  (interactive)
  (setf *ldoc-groups* (make-hash-table :test 'equal)))

(when (null *ldoc-groups*)
  (ldoc-init-groups))

(defun ldoc-add-group (&rest args)
  (let ((group (apply #'ldoc-create-group args)))
    (setf (gethash (ldoc-group-name group)
		   *ldoc-groups*)
	  group)))

(defun ldoc-delete-group (name)
  (remhash name *ldoc-groups*))

(defun* ldoc-scan-group-files (&optional (group-name (ldoc-choose-group)))
  (ldoc-load-group-files (gethash group-name *ldoc-groups*))
  (message "Scanned group %s for files." group-name))

;;; Selecting and deselecting groups

(defun* ldoc-choose-group () 
  (completing-read "Choose a group: " *ldoc-groups* nil :require-match))

(defun* ldoc-select-group (&optional (group-name (ldoc-choose-group)))
  (interactive)
  (let ((group (gethash group-name *ldoc-groups*)))
    (prog1 group 
      (setf (ldoc-group-selected-p group) t))))

(defun* ldoc-deselect-group (&optional (group-name (ldoc-choose-group)))
  (interactive)
  (let ((group (gethash group-name *ldoc-groups*)))
    (prog1 group 
      (setf (ldoc-group-selected-p group) nil))))

(defun ldoc-get-selected-groups ()
  (let (groups)
    (maphash #'(lambda (name group)
		 (declare (ignore name))
		 (when (ldoc-group-selected-p group)
		   (push group groups)))
	     *ldoc-groups*)
    groups))

;;; Printing information about groups

(defun* ldoc-describe-group (&optional (group-name (ldoc-choose-group)))
  (interactive)
  (let ((group (gethash group-name *ldoc-groups*)))
    (message (ldoc-group-description group))))

(defun ldoc-show-selected-groups ()
  (interactive)
  (maphash #'(lambda (name group)
	       (ldoc-describe-group name))
	   *ldoc-groups*))

;;; Scanning definitions and docstrings

;; A definition line is defined by the regexp `ldoc-def-regexp' below. 
;; A definition's docstring (if any) is found like this: 
;; 
;;   - Scan lines for any definition line
;;   - Find and parse definition
;;   - Scan following lines

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
				"\\(" 
				(regexp-opt ldoc-def-types)
				"\\)"
				" \\([^\n ]*\\)"))


;; (defun golisp-write-def (target line anchor-p)	
;;   (string-match golisp-def-regexp line)
;;   (let ((type (match-string 1 line)))
;;     (when type
;;       (let ((pos (position-if (lambda (x)
;; 				(string= type x))
;; 			      golisp-def-types)))
;; 	(when pos
;; 	  (if anchor-p
;; 	      (concat "(" type " " target " ")
;; 	    (list 
;; 	     (concat "<" "(" (nth pos golisp-def-names) " " target ")" ">")
;; 	     (concat "(" type " " target " "))))))))



(provide 'ldoc)
;;; ldoc.el ends here
