;;; typescript-tsx-mode.el --- summary -*- lexical-binding: t -*-

;; Author: Adam Hall
;; Maintainer: Adam Hall
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
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

;; Used to get nice highlighting with tree sitter. Need to get indent
;; to work and start adding cool functions from web-mode

;;; Code:
;; (require 'typescript-mode)
(require 'tree-sitter)
(require 'tree-sitter-hl)
(require 'tree-sitter-langs)
(require 'tree-sitter-indent)

(defcustom typescript-tsx-indent-offset 2
  "Indent offset for typescript-tsx-mode."
  :type 'integer
  :group 'typescript-tsx)

(defvar tree-sitter-indent-typescript-tsx-scopes
  '((indent-all .
		;; these nodes are always indented
		(
		 ;; arguments
		 ;; statement_block
		 ;; formal_parameters



		 ;; ;; jsx_text
		 ;; jsx_attribute

		 ;; TODO this doesnt indent till after created


		 "." 			; dot chaining

		 ))
    (indent-rest . ;; if parent node is one of this and node is not
		 ;; first → indent
		 (
		  ;; arrow_function

		  ;; jsx_opening_element
		  jsx_element

		  import_statement
		  export_statement

		  ;; export_clause
		  ;; import_clause

		  object_pattern
		  array_pattern

		  object
		  array

		  ;; export_clause
		  ))
    (indent-body . ;; if parent node is one of this and current node
		   ;; is in middle → indent
		 (
		  statement_block
		  formal_parameters
		  ))
    
    (paren-indent . ;; if parent node is one of these → indent to
		    ;; paren opener not done in the js world
		  ())
    ;; (align-char-to . ;; chaining char → node types we move parentwise
    ;; 		     ;; to find the first chaining char
    ;; 		   ((?. . (member_expression))))
    (aligned-siblings . ;; siblings (nodes with same parent) should be
			;; aligned to the first child
		      (
		       ;; function arguments
		       required_parameter
		       optional_parameter

		       ;; export_clause

		       ;; destructured
		       ;; object_pattern
		       shorthand_property_identifier_pattern
		       ;; pair_pattern
		       ;; jsx_attribute

		       ))

    (multi-line-text . ;; if node is one of this, then don't modify
		       ;; the indent this is basically a peaceful way
		       ;; out by saying "this looks like something
		       ;; that cannot be indented using AST, so best I
		       ;; leave it as-is"
		     (comment))
    (outdent . ;; these nodes always outdent (1 shift in opposite direction)
	     ("}" ")" "]"
	      jsx_closing_element)))
  "Scopes for indenting in typescript-tsx.")

;;;###autoload
(define-derived-mode typescript-tsx-mode prog-mode "typescript-tsx"
  "Major mode for editing TSX files.

WIP trying to create a major mode based off tree-sitter."
  :group 'typescript-tsx

  (setq-local indent-line-function #'tree-sitter-indent-line)

  (setq font-lock-defaults '(nil))

  (setq-local comment-start "// ")
  (setq-local comment-start-skip "\\(?://+\\|/\\*+\\)\\s *")
  (setq-local comment-end "")

  ;; (tree-sitter-require 'tsx)
  (tree-sitter-indent-mode)
  (tree-sitter-hl-mode))

(add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx))

;; TODO not working yet

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . typescript-tsx-mode))

(provide 'typescript-tsx-mode)

;;; typescript-tsx-mode.el ends here
