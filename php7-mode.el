;;; php7-mode.el --- Major mode for editing PHP7  -*- lexical-binding: t -*-

;; Copyright (C) 2008-2017 Free Software Foundation, Inc.

;; Author: Karl Landstrom <karl.landstrom@brgeight.se>
;;         Daniel Colascione <dan.colascione@gmail.com>
;;         USAMI Kenta <tadsan@zonu.me>
;; Maintainer: USAMI Kenta <tadsan@zonu.me>
;; Created: 28 Sep 2017
;; Version: 0.0.4
;; Keywords: languages php
;; URL: https://github.com/emacs-php/php7-mode
;; Package-Requires: ((emacs "25") (cl-lib "0.5"))

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is based on Karl Landstrom's barebones javascript-mode.
;; This is much more robust and works with cc-mode's comment filling
;; (mostly).
;;
;; The main features of this PHP mode are syntactic
;; highlighting (enabled with `font-lock-mode' or
;; `global-font-lock-mode'), automatic indentation and filling of
;; comments, C preprocessor fontification, and MozRepl integration.
;;
;; General Remarks:
;;
;; XXX: This mode assumes that block comments are not nested inside block
;; XXX: comments
;;
;; Exported names start with "php7-"; private names start with
;; "php7--".

;;; Code:


(require 'cc-mode)
(require 'newcomment)
(require 'thingatpt)                    ; forward-symbol etc
(require 'imenu)
(require 'sgml-mode)
(require 'prog-mode)

(eval-when-compile
  (require 'cl-lib)
  (require 'ido))

(defvar electric-layout-rules)
(declare-function ido-mode "ido" (&optional arg))

;;; Constants

(defconst php7--name-start-re "[a-zA-Z_$]"
  "Regexp matching the start of a PHP identifier, without grouping.")

(defconst php7--stmt-delim-chars "^;{}?:")

(defconst php7--name-re (concat php7--name-start-re
                              "\\(?:\\s_\\|\\sw\\)*")
  "Regexp matching a PHP identifier, without grouping.")

(defconst php7--objfield-re (concat php7--name-re ":")
  "Regexp matching the start of a JavaScript object field.")

(defconst php7--dotted-name-re
  (concat php7--name-re "\\(?:\\." php7--name-re "\\)*")
  "Regexp matching a dot-separated sequence of JavaScript names.")

(defconst php7--cpp-name-re php7--name-re
  "Regexp matching a C preprocessor name.")

(defconst php7--opt-cpp-start "^\\s-*#\\s-*\\([[:alnum:]]+\\)"
  "Regexp matching the prefix of a cpp directive.
This includes the directive name, or nil in languages without
preprocessor support.  The first submatch surrounds the directive
name.")

(defconst php7--plain-method-re
  (concat "^\\s-*?\\(" php7--dotted-name-re "\\)\\.prototype"
          "\\.\\(" php7--name-re "\\)\\s-*?=\\s-*?\\(function\\)\\_>")
  "Regexp matching an explicit JavaScript prototype \"method\" declaration.
Group 1 is a (possibly-dotted) class name, group 2 is a method name,
and group 3 is the `function' keyword.")

(defconst php7--function-heading-1-re
  (concat
   "^\\s-*function\\(?:\\s-\\|\\*\\)+\\(" php7--name-re "\\)")
  "Regexp matching the start of a PHP function header.
Match group 1 is the name of the function.")

(defconst php7--function-heading-2-re
  (concat
   "^\\s-*\\(" php7--name-re "\\)\\s-*:\\s-*function\\_>")
  "Regexp matching the start of a function entry in an associative array.
Match group 1 is the name of the function.")

(defconst php7--function-heading-3-re
  (concat
   "^\\s-*\\(?:var\\s-+\\)?\\(" php7--dotted-name-re "\\)"
   "\\s-*=\\s-*function\\_>")
  "Regexp matching a line in the JavaScript form \"var MUMBLE = function\".
Match group 1 is MUMBLE.")

(defconst php7--macro-decl-re
  (concat "^\\s-*#\\s-*define\\s-+\\(" php7--cpp-name-re "\\)\\s-*(")
  "Regexp matching a CPP macro definition, up to the opening parenthesis.
Match group 1 is the name of the macro.")

(defun php7--regexp-opt-symbol (list)
  "Like `regexp-opt', but surround the result with `\\\\_<' and `\\\\_>'."
  (concat "\\_<" (regexp-opt list t) "\\_>"))

(defconst php7--keyword-re
  (php7--regexp-opt-symbol
   '("__halt_compiler" "abstract" "as" "break" "case" "catch" "class" "clone"
     "const" "continue" "declare" "default" "die" "do" "echo" "else" "elseif"
     "empty" "enddeclare" "endfor" "endforeach" "endif" "endswitch" "endwhile"
     "eval" "exit" "extends" "final" "finally" "for" "foreach" "function"
     "global" "goto" "if" "implements" "include" "include_once" "instanceof"
     "insteadof" "interface" "isset" "list" "namespace" "new" "print" "private"
     "protected" "public" "require" "require_once" "return" "static" "switch"
     "throw" "try" "unset" "use" "var" "yield" "while"))
  "Regexp matching any PHP keyword.")

(defconst php7--basic-type-re
  (php7--regexp-opt-symbol
   '("bool" "boolean" "byte" "char" "double" "float" "int" "iterable" "long"
     "short" "string" "resource" "object" "mixed" "numeric" "void"))
  "Regular expression matching any predefined type in PHP.")

(defconst php7--constant-re
  (php7--regexp-opt-symbol
   '("false" "null" "true" "__LINE__" "__FILE__" "__DIR__" "__FUNCTION__"
     "__CLASS__" "__TRAIT__" "__METHOD__" "__NAMESPACE__"))
  "Regular expression matching any future reserved words in PHP.")


(defconst php7--font-lock-keywords-1
  (list
   "\\_<import\\_>"
   (list php7--function-heading-1-re 1 font-lock-function-name-face)
   (list php7--function-heading-2-re 1 font-lock-function-name-face))
  "Level one font lock keywords for `php7-mode'.")

(defconst php7--font-lock-keywords-2
  (append php7--font-lock-keywords-1
          (list (list php7--keyword-re 1 font-lock-keyword-face)
                (list "\\_<for\\_>"
                      "\\s-+\\(each\\)\\_>" nil nil
                      (list 1 'font-lock-keyword-face))
                (cons php7--basic-type-re font-lock-type-face)
                (cons php7--constant-re font-lock-constant-face)))
  "Level two font lock keywords for `php7-mode'.")

;; php7--pitem is the basic building block of the lexical
;; database. When one refers to a real part of the buffer, the region
;; of text to which it refers is split into a conceptual header and
;; body. Consider the (very short) block described by a hypothetical
;; php7--pitem:
;;
;;   function foo(a,b,c) { return 42; }
;;   ^                    ^            ^
;;   |                    |            |
;;   +- h-begin           +- h-end     +- b-end
;;
;; (Remember that these are buffer positions, and therefore point
;; between characters, not at them. An arrow drawn to a character
;; indicates the corresponding position is between that character and
;; the one immediately preceding it.)
;;
;; The header is the region of text [h-begin, h-end], and is
;; the text needed to unambiguously recognize the start of the
;; construct. If the entire header is not present, the construct is
;; not recognized at all. No other pitems may be nested inside the
;; header.
;;
;; The body is the region [h-end, b-end]. It may contain nested
;; php7--pitem instances. The body of a pitem may be empty: in
;; that case, b-end is equal to header-end.
;;
;; The three points obey the following relationship:
;;
;;   h-begin < h-end <= b-end
;;
;; We put a text property in the buffer on the character *before*
;; h-end, and if we see it, on the character *before* b-end.
;;
;; The text property for h-end, php7--pstate, is actually a list
;; of all php7--pitem instances open after the marked character.
;;
;; The text property for b-end, php7--pend, is simply the
;; php7--pitem that ends after the marked character. (Because
;; pitems always end when the paren-depth drops below a critical
;; value, and because we can only drop one level per character, only
;; one pitem may end at a given character.)
;;
;; In the structure below, we only store h-begin and (sometimes)
;; b-end. We can trivially and quickly find h-end by going to h-begin
;; and searching for an php7--pstate text property. Since no other
;; php7--pitem instances can be nested inside the header of a
;; pitem, the location after the character with this text property
;; must be h-end.
;;
;; php7--pitem instances are never modified (with the exception
;; of the b-end field). Instead, modified copies are added at
;; subsequence parse points.
;; (The exception for b-end and its caveats is described below.)
;;

(cl-defstruct (php7--pitem (:type list))
  ;; IMPORTANT: Do not alter the position of fields within the list.
  ;; Various bits of code depend on their positions, particularly
  ;; anything that manipulates the list of children.

  ;; List of children inside this pitem's body
  (children nil :read-only t)

  ;; When we reach this paren depth after h-end, the pitem ends
  (paren-depth nil :read-only t)

  ;; Symbol or class-style plist if this is a class
  (type nil :read-only t)

  ;; See above
  (h-begin nil :read-only t)

  ;; List of strings giving the parts of the name of this pitem (e.g.,
  ;; '("MyClass" "myMethod"), or t if this pitem is anonymous
  (name nil :read-only t)

  ;; THIS FIELD IS MUTATED, and its value is shared by all copies of
  ;; this pitem: when we copy-and-modify pitem instances, we share
  ;; their tail structures, so all the copies actually have the same
  ;; terminating cons cell. We modify that shared cons cell directly.
  ;;
  ;; The field value is either a number (buffer location) or nil if
  ;; unknown.
  ;;
  ;; If the field's value is greater than `php7--cache-end', the
  ;; value is stale and must be treated as if it were nil. Conversely,
  ;; if this field is nil, it is guaranteed that this pitem is open up
  ;; to at least `php7--cache-end'. (This property is handy when
  ;; computing whether we're inside a given pitem.)
  ;;
  (b-end nil))

;; The pitem we start parsing with.
(defconst php7--initial-pitem
  (make-php7--pitem
   :paren-depth most-negative-fixnum
   :type 'toplevel))

;;; User Customization

(defgroup php7-mode nil
  "Customization variables for Php7 mode."
  :tag "Php7"
  :group 'languages)

(defcustom php7-indent-level 4
  "Number of spaces for each indentation step in `php7-mode'."
  :type 'integer
  :safe 'integerp
  :group 'php7-mode)

(defcustom php7-expr-indent-offset 0
  "Number of additional spaces for indenting continued expressions.
The value must be no less than minus `php7-indent-level'."
  :type 'integer
  :safe 'integerp
  :group 'php7-mode)

(defcustom php7-paren-indent-offset 0
  "Number of additional spaces for indenting expressions in parentheses.
The value must be no less than minus `php7-indent-level'."
  :type 'integer
  :safe 'integerp
  :group 'php7-mode)

(defcustom php7-square-indent-offset 0
  "Number of additional spaces for indenting expressions in square braces.
The value must be no less than minus `php7-indent-level'."
  :type 'integer
  :safe 'integerp
  :group 'php7-mode)

(defcustom php7-curly-indent-offset 0
  "Number of additional spaces for indenting expressions in curly braces.
The value must be no less than minus `php7-indent-level'."
  :type 'integer
  :safe 'integerp
  :group 'php7-mode)

(defcustom php7-switch-indent-offset 0
  "Number of additional spaces for indenting the contents of a switch block.
The value must not be negative."
  :type 'integer
  :safe 'integerp
  :group 'php7-mode
  :version "24.4")

(defcustom php7-flat-functions nil
  "Treat nested functions as top-level functions in `php7-mode'.
This applies to function movement, marking, and so on."
  :type 'boolean
  :group 'php7-mode)

(defcustom php7-indent-align-list-continuation t
  "Align continuation of non-empty ([{ lines in `php7-mode'."
  :type 'boolean
  :group 'php7-mode)

(defcustom php7-comment-lineup-func #'c-lineup-C-comments
  "Lineup function for `cc-mode-style', for C comments in `php7-mode'."
  :type 'function
  :group 'php7-mode)

(defcustom php7-indent-first-init nil
  "Non-nil means specially indent the first variable declaration's initializer.
Normally, the first declaration's initializer is unindented, and
subsequent declarations have their identifiers aligned with it:

  var o = {
      foo: 3
  };

  var o = {
      foo: 3
  },
      bar = 2;

If this option has the value t, indent the first declaration's
initializer by an additional level:

  var o = {
          foo: 3
      };

  var o = {
          foo: 3
      },
      bar = 2;

If this option has the value `dynamic', if there is only one declaration,
don't indent the first one's initializer; otherwise, indent it.

  var o = {
      foo: 3
  };

  var o = {
          foo: 3
      },
      bar = 2;"
  :type '(choice (const nil) (const t) (const dynamic))
  :safe 'symbolp
  :group 'php7-mode)

(defcustom php7-chain-indent nil
  "Use \"chained\" indentation.
Chained indentation applies when the current line starts with \".\".
If the previous expression also contains a \".\" at the same level,
then the \".\"s will be lined up:

  let x = svg.mumble()
             .chained;
"
  :type 'boolean
  :safe 'booleanp
  :group 'php7-mode)

;;; KeyMap

(defvar php7-mode-map
  (let ((keymap (make-sparse-keymap)))
    keymap)
  "Keymap for `php7-mode'.")

;;; Syntax table and parsing

(defvar php7-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?` "\"" table)
    table)
  "Syntax table for `php7-mode'.")

(defvar php7--quick-match-re nil
  "Autogenerated regexp used by `php7-mode' to match buffer constructs.")

(defvar php7--quick-match-re-func nil
  "Autogenerated regexp used by `php7-mode' to match constructs and functions.")

(make-variable-buffer-local 'php7--quick-match-re)
(make-variable-buffer-local 'php7--quick-match-re-func)

(defvar php7--cache-end 1
  "Last valid buffer position for the `php7-mode' function cache.")
(make-variable-buffer-local 'php7--cache-end)

(defvar php7--last-parse-pos nil
  "Latest parse position reached by `php7--ensure-cache'.")
(make-variable-buffer-local 'php7--last-parse-pos)

(defvar php7--state-at-last-parse-pos nil
  "Parse state at `php7--last-parse-pos'.")
(make-variable-buffer-local 'php7--state-at-last-parse-pos)

(defun php7--flatten-list (list)
  (cl-loop for item in list
           nconc (cond ((consp item)
                        (php7--flatten-list item))
                       (item (list item)))))

(defun php7--maybe-join (prefix separator suffix &rest list)
  "Helper function for `php7--update-quick-match-re'.
If LIST contains any element that is not nil, return its non-nil
elements, separated by SEPARATOR, prefixed by PREFIX, and ended
with SUFFIX as with `concat'.  Otherwise, if LIST is empty, return
nil.  If any element in LIST is itself a list, flatten that
element."
  (setq list (php7--flatten-list list))
  (when list
    (concat prefix (mapconcat #'identity list separator) suffix)))

(defun php7--update-quick-match-re ()
  "Internal function used by `php7-mode' for caching buffer constructs.
This updates `php7--quick-match-re', based on the current set of
enabled frameworks."
  (setq php7--quick-match-re
        (php7--maybe-join
         "^[ \t]*\\(?:" "\\|" "\\)"

         ;; #define mumble
         "#define[ \t]+[a-zA-Z_]"

          ;; var mumble = THING (
         (php7--maybe-join
          "\\(?:var[ \t]+\\)?[a-zA-Z_$0-9.]+[ \t]*=[ \t]*\\(?:"
          "\\|"
          "\\)[ \t]*(")


         ;; mumble.prototypeTHING
         (php7--maybe-join
          "[a-zA-Z_$0-9.]+\\.prototype\\(?:" "\\|" "\\)")))

  (setq php7--quick-match-re-func
        (concat "function\\|" php7--quick-match-re)))

(defun php7--forward-text-property (propname)
  "Move over the next value of PROPNAME in the buffer.
If found, return that value and leave point after the character
having that value; otherwise, return nil and leave point at EOB."
  (let ((next-value (get-text-property (point) propname)))
    (if next-value
        (forward-char)

      (goto-char (next-single-property-change
                  (point) propname nil (point-max)))
      (unless (eobp)
        (setq next-value (get-text-property (point) propname))
        (forward-char)))

    next-value))

(defun php7--backward-text-property (propname)
  "Move over the previous value of PROPNAME in the buffer.
If found, return that value and leave point just before the
character that has that value, otherwise return nil and leave
point at BOB."
    (unless (bobp)
      (let ((prev-value (get-text-property (1- (point)) propname)))
        (if prev-value
            (backward-char)

          (goto-char (previous-single-property-change
                      (point) propname nil (point-min)))

          (unless (bobp)
            (backward-char)
            (setq prev-value (get-text-property (point) propname))))

        prev-value)))

(defsubst php7--forward-pstate ()
  (php7--forward-text-property 'php7--pstate))

(defsubst php7--backward-pstate ()
  (php7--backward-text-property 'php7--pstate))

(defun php7--pitem-goto-h-end (pitem)
  (goto-char (php7--pitem-h-begin pitem))
  (php7--forward-pstate))

(defun php7--re-search-forward-inner (regexp &optional bound count)
  "Helper function for `php7--re-search-forward'."
  (let ((parse)
        str-terminator
        (orig-macro-end (save-excursion
                          (when (php7--beginning-of-macro)
                            (c-end-of-macro)
                            (point)))))
    (while (> count 0)
      (re-search-forward regexp bound)
      (setq parse (syntax-ppss))
      (cond ((setq str-terminator (nth 3 parse))
             (when (eq str-terminator t)
               (setq str-terminator ?/))
             (re-search-forward
              (concat "\\([^\\]\\|^\\)" (string str-terminator))
              (point-at-eol) t))
            ((nth 7 parse)
             (forward-line))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?\/) (eq (char-after) ?\*)))
             (re-search-forward "\\*/"))
            ((and (not (and orig-macro-end
                            (<= (point) orig-macro-end)))
                  (php7--beginning-of-macro))
             (c-end-of-macro))
            (t
             (setq count (1- count))))))
  (point))


(defun php7--re-search-forward (regexp &optional bound noerror count)
  "Search forward, ignoring strings, cpp macros, and comments.
This function invokes `re-search-forward', but treats the buffer
as if strings, cpp macros, and comments have been removed.

If invoked while inside a macro, it treats the contents of the
macro as normal text."
  (unless count (setq count 1))
  (let ((saved-point (point))
        (search-fun
         (cond ((< count 0) (setq count (- count))
                #'php7--re-search-backward-inner)
               ((> count 0) #'php7--re-search-forward-inner)
               (t #'ignore))))
    (condition-case err
        (funcall search-fun regexp bound count)
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (signal (car err) (cdr err)))))))


(defun php7--re-search-backward-inner (regexp &optional bound count)
  "Auxiliary function for `php7--re-search-backward'."
  (let ((parse)
        str-terminator
        (orig-macro-start
         (save-excursion
           (and (php7--beginning-of-macro)
                (point)))))
    (while (> count 0)
      (re-search-backward regexp bound)
      (when (and (> (point) (point-min))
                 (save-excursion (backward-char) (looking-at "/[/*]")))
        (forward-char))
      (setq parse (syntax-ppss))
      (cond ((setq str-terminator (nth 3 parse))
             (when (eq str-terminator t)
               (setq str-terminator ?/))
             (re-search-backward
              (concat "\\([^\\]\\|^\\)" (string str-terminator))
              (point-at-bol) t))
            ((nth 7 parse)
             (goto-char (nth 8 parse)))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?/) (eq (char-after) ?*)))
             (re-search-backward "/\\*"))
            ((and (not (and orig-macro-start
                            (>= (point) orig-macro-start)))
                  (php7--beginning-of-macro)))
            (t
             (setq count (1- count))))))
  (point))


(defun php7--re-search-backward (regexp &optional bound noerror count)
  "Search backward, ignoring strings, preprocessor macros, and comments.

This function invokes `re-search-backward' but treats the buffer
as if strings, preprocessor macros, and comments have been
removed.

If invoked while inside a macro, treat the macro as normal text."
  (php7--re-search-forward regexp bound noerror (if count (- count) -1)))

(defun php7--forward-expression ()
  "Move forward over a whole PHP expression.
This function doesn't move over expressions continued across
lines."
  (cl-loop
   ;; non-continued case; simplistic, but good enough?
   do (cl-loop until (or (eolp)
                         (progn
                           (forward-comment most-positive-fixnum)
                           (memq (char-after) '(?\, ?\; ?\] ?\) ?\}))))
               do (forward-sexp))

   while (and (eq (char-after) ?\n)
              (save-excursion
                (forward-char)
                (php7--continued-expression-p)))))

(defun php7--forward-function-decl ()
  "Move forward over a PHP function declaration.
This puts point at the `function' keyword.

If this is a syntactically-correct non-expression function,
return the name of the function, or t if the name could not be
determined.  Otherwise, return nil."
  (cl-assert (looking-at "\\_<function\\_>"))
  (let ((name t))
    (forward-word-strictly)
    (forward-comment most-positive-fixnum)
    (when (eq (char-after) ?*)
      (forward-char)
      (forward-comment most-positive-fixnum))
    (when (looking-at php7--name-re)
      (setq name (match-string-no-properties 0))
      (goto-char (match-end 0)))
    (forward-comment most-positive-fixnum)
    (and (eq (char-after) ?\( )
         (ignore-errors (forward-list) t)
         (progn (forward-comment most-positive-fixnum)
                (and (eq (char-after) ?{)
                     name)))))

(defun php7--function-prologue-beginning (&optional pos)
  "Return the start of the PHP function prologue containing POS.
A function prologue is everything from start of the definition up
to and including the opening brace.  POS defaults to point.
If POS is not in a function prologue, return nil."
  (let (prologue-begin)
    (save-excursion
      (if pos
          (goto-char pos)
        (setq pos (point)))

      (when (save-excursion
              (forward-line 0)
              (or (looking-at php7--function-heading-2-re)
                  (looking-at php7--function-heading-3-re)))

        (setq prologue-begin (match-beginning 1))
        (when (<= prologue-begin pos)
          (goto-char (match-end 0))))

      (skip-syntax-backward "w_")
      (and (or (looking-at "\\_<function\\_>")
               (php7--re-search-backward "\\_<function\\_>" nil t))

           (save-match-data (goto-char (match-beginning 0))
                            (php7--forward-function-decl))

           (<= pos (point))
           (or prologue-begin (match-beginning 0))))))

(defun php7--beginning-of-defun-raw ()
  "Helper function for `php7-beginning-of-defun'.
Go to previous defun-beginning and return the parse state for it,
or nil if we went all the way back to bob and don't find
anything."
  (php7--ensure-cache)
  (let (pstate)
    (while (and (setq pstate (php7--backward-pstate))
                (not (eq 'function (php7--pitem-type (car pstate))))))
    (and (not (bobp)) pstate)))

(defun php7--pstate-is-toplevel-defun (pstate)
  "Helper function for `php7--beginning-of-defun-nested'.
If PSTATE represents a non-empty top-level defun, return the
top-most pitem.  Otherwise, return nil."
  (cl-loop for pitem in pstate
           with func-depth = 0
           with func-pitem
           if (eq 'function (php7--pitem-type pitem))
           do (cl-incf func-depth)
           and do (setq func-pitem pitem)
           finally return (if (eq func-depth 1) func-pitem)))

(defun php7--beginning-of-defun-nested ()
  "Helper function for `php7--beginning-of-defun'.
Return the pitem of the function we went to the beginning of."
  (or
   ;; Look for the smallest function that encloses point...
   (cl-loop for pitem in (php7--parse-state-at-point)
            if (and (eq 'function (php7--pitem-type pitem))
                    (php7--inside-pitem-p pitem))
            do (goto-char (php7--pitem-h-begin pitem))
            and return pitem)

   ;; ...and if that isn't found, look for the previous top-level
   ;; defun
   (cl-loop for pstate = (php7--backward-pstate)
            while pstate
            if (php7--pstate-is-toplevel-defun pstate)
            do (goto-char (php7--pitem-h-begin it))
            and return it)))

(defun php7--beginning-of-defun-flat ()
  "Helper function for `php7-beginning-of-defun'."
  (let ((pstate (php7--beginning-of-defun-raw)))
    (when pstate
      (goto-char (php7--pitem-h-begin (car pstate))))))

(defun php7-beginning-of-defun (&optional arg)
  "Value of `beginning-of-defun-function' for `php7-mode'."
  (setq arg (or arg 1))
  (while (and (not (eobp)) (< arg 0))
    (cl-incf arg)
    (when (and (not php7-flat-functions)
               (or (eq (php7-syntactic-context) 'function)
                   (php7--function-prologue-beginning)))
      (php7-end-of-defun))

    (if (php7--re-search-forward
         "\\_<function\\_>" nil t)
        (goto-char (php7--function-prologue-beginning))
      (goto-char (point-max))))

  (while (> arg 0)
    (cl-decf arg)
    ;; If we're just past the end of a function, the user probably wants
    ;; to go to the beginning of *that* function
    (when (eq (char-before) ?})
      (backward-char))

    (let ((prologue-begin (php7--function-prologue-beginning)))
      (cond ((and prologue-begin (< prologue-begin (point)))
             (goto-char prologue-begin))

            (php7-flat-functions
             (php7--beginning-of-defun-flat))
            (t
             (php7--beginning-of-defun-nested))))))

(defun php7--flush-caches (&optional beg ignored)
  "Flush the `php7-mode' syntax cache after position BEG.
BEG defaults to `point-min', meaning to flush the entire cache."
  (interactive)
  (setq beg (or beg (save-restriction (widen) (point-min))))
  (setq php7--cache-end (min php7--cache-end beg)))

(defmacro php7--debug (&rest _arguments)
  ;; `(message ,@arguments)
  )

(defun php7--ensure-cache--pop-if-ended (open-items paren-depth)
  (let ((top-item (car open-items)))
    (when (<= paren-depth (php7--pitem-paren-depth top-item))
      (cl-assert (not (get-text-property (1- (point)) 'php7-pend)))
      (put-text-property (1- (point)) (point) 'php7--pend top-item)
      (setf (php7--pitem-b-end top-item) (point))
      (setq open-items
            ;; open-items must contain at least two items for this to
            ;; work, but because we push a dummy item to start with,
            ;; that assumption holds.
            (cons (php7--pitem-add-child (cl-second open-items) top-item)
                  (cddr open-items)))))
  open-items)

(defmacro php7--ensure-cache--update-parse ()
  "Helper function for `php7--ensure-cache'.
Update parsing information up to point, referring to parse,
prev-parse-point, goal-point, and open-items bound lexically in
the body of `php7--ensure-cache'."
  `(progn
     (setq goal-point (point))
     (goto-char prev-parse-point)
     (while (progn
              (setq open-items (php7--ensure-cache--pop-if-ended
                                open-items (car parse)))
              ;; Make sure parse-partial-sexp doesn't stop because we *entered*
              ;; the given depth -- i.e., make sure we're deeper than the target
              ;; depth.
              (cl-assert (> (nth 0 parse)
                         (php7--pitem-paren-depth (car open-items))))
              (setq parse (parse-partial-sexp
                           prev-parse-point goal-point
                           (php7--pitem-paren-depth (car open-items))
                           nil parse))

;;              (let ((overlay (make-overlay prev-parse-point (point))))
;;                (overlay-put overlay 'face '(:background "red"))
;;                (unwind-protect
;;                     (progn
;;                       (php7--debug "parsed: %S" parse)
;;                       (sit-for 1))
;;                  (delete-overlay overlay)))

              (setq prev-parse-point (point))
              (< (point) goal-point)))

     (setq open-items (php7--ensure-cache--pop-if-ended
                       open-items (car parse)))))

(defun php7--show-cache-at-point ()
  (interactive)
  (require 'pp)
  (let ((prop (get-text-property (point) 'php7--pstate)))
    (with-output-to-temp-buffer "*Help*"
      (pp prop))))

(defun php7--split-name (string)
  "Split a JavaScript name into its dot-separated parts.
This also removes any prototype parts from the split name
\(unless the name is just \"prototype\" to start with)."
  (let ((name (save-match-data
                (split-string string "\\." t))))
    (unless (and (= (length name) 1)
                 (equal (car name) "prototype"))

      (setq name (remove "prototype" name)))))

(defvar php7--guess-function-name-start nil)

(defun php7--guess-function-name (position)
  "Guess the name of the PHP function at POSITION.
POSITION should be just after the end of the word \"function\".
Return the name of the function, or nil if the name could not be
guessed.

This function clobbers match data.  If we find the preamble
begins earlier than expected while guessing the function name,
set `php7--guess-function-name-start' to that position; otherwise,
set that variable to nil."
  (setq php7--guess-function-name-start nil)
  (save-excursion
    (goto-char position)
    (forward-line 0)
    (cond
     ((looking-at php7--function-heading-3-re)
      (and (eq (match-end 0) position)
           (setq php7--guess-function-name-start (match-beginning 1))
           (match-string-no-properties 1)))

     ((looking-at php7--function-heading-2-re)
      (and (eq (match-end 0) position)
           (setq php7--guess-function-name-start (match-beginning 1))
           (match-string-no-properties 1))))))

(defun php7--clear-stale-cache ()
  ;; Clear any endings that occur after point
  (let (end-prop)
    (save-excursion
      (while (setq end-prop (php7--forward-text-property
                             'php7--pend))
        (setf (php7--pitem-b-end end-prop) nil))))

  ;; Remove any cache properties after this point
  (remove-text-properties (point) (point-max)
                          '(php7--pstate t php7--pend t)))

(defun php7--ensure-cache (&optional limit)
  "Ensures brace cache is valid up to the character before LIMIT.
LIMIT defaults to point."
  (setq limit (or limit (point)))
  (when (< php7--cache-end limit)

    (c-save-buffer-state
        (open-items
         parse
         prev-parse-point
         name
         case-fold-search
         filtered-class-styles
         goal-point)

      ;; Figure out which class styles we need to look for
      (setq filtered-class-styles
            (cl-loop for style in '() ; php7--class-styles
                     if (memq (plist-get style :framework)
                              '()) ; php7-enabled-frameworks
                     collect style))

      (save-excursion
        (save-restriction
          (widen)

          ;; Find last known good position
          (goto-char php7--cache-end)
          (unless (bobp)
            (setq open-items (get-text-property
                              (1- (point)) 'php7--pstate))

            (unless open-items
              (goto-char (previous-single-property-change
                          (point) 'php7--pstate nil (point-min)))

              (unless (bobp)
                (setq open-items (get-text-property (1- (point))
                                                    'php7--pstate))
                (cl-assert open-items))))

          (unless open-items
            ;; Make a placeholder for the top-level definition
            (setq open-items (list php7--initial-pitem)))

          (setq parse (syntax-ppss))
          (setq prev-parse-point (point))

          (php7--clear-stale-cache)

          (narrow-to-region (point-min) limit)

          (cl-loop while (re-search-forward php7--quick-match-re-func nil t)
                   for orig-match-start = (goto-char (match-beginning 0))
                   for orig-match-end = (match-end 0)
                   do (php7--ensure-cache--update-parse)
                   for orig-depth = (nth 0 parse)

                   ;; Each of these conditions should return non-nil if
                   ;; we should add a new item and leave point at the end
                   ;; of the new item's header (h-end in the
                   ;; php7--pitem diagram). This point is the one
                   ;; after the last character we need to unambiguously
                   ;; detect this construct. If one of these evaluates to
                   ;; nil, the location of the point is ignored.
                   if (cond
                       ;; In comment or string
                       ((nth 8 parse) nil)

                       ;; Regular function declaration
                       ((and (looking-at "\\_<function\\_>")
                             (setq name (php7--forward-function-decl)))

                        (when (eq name t)
                          (setq name (php7--guess-function-name orig-match-end))
                          (if name
                              (when php7--guess-function-name-start
                                (setq orig-match-start
                                      php7--guess-function-name-start))

                            (setq name t)))

                        (cl-assert (eq (char-after) ?{))
                        (forward-char)
                        (make-php7--pitem
                         :paren-depth orig-depth
                         :h-begin orig-match-start
                         :type 'function
                         :name (if (eq name t)
                                   name
                                 (php7--split-name name))))

                       ;; Macro
                       ((looking-at php7--macro-decl-re)

                        ;; Macros often contain unbalanced parentheses.
                        ;; Make sure that h-end is at the textual end of
                        ;; the macro no matter what the parenthesis say.
                        (c-end-of-macro)
                        (php7--ensure-cache--update-parse)

                        (make-php7--pitem
                         :paren-depth (nth 0 parse)
                         :h-begin orig-match-start
                         :type 'macro
                         :name (list (match-string-no-properties 1))))

                       ;; "Prototype function" declaration
                       ((looking-at php7--plain-method-re)
                        (goto-char (match-beginning 3))
                        (when (save-match-data
                                (php7--forward-function-decl))
                          (forward-char)
                          (make-php7--pitem
                           :paren-depth orig-depth
                           :h-begin orig-match-start
                           :type 'function
                           :name (nconc (php7--split-name
                                         (match-string-no-properties 1))
                                        (list (match-string-no-properties 2))))))

                       ;; Class definition
                       ((cl-loop
                         with syntactic-context =
                         (php7--syntactic-context-from-pstate open-items)
                         for class-style in filtered-class-styles
                         if (and (memq syntactic-context
                                       (plist-get class-style :contexts))
                                 (looking-at (plist-get class-style
                                                        :class-decl)))
                         do (goto-char (match-end 0))
                         and return
                         (make-php7--pitem
                          :paren-depth orig-depth
                          :h-begin orig-match-start
                          :type class-style
                          :name (php7--split-name
                                 (match-string-no-properties 1))))))

                   do (php7--ensure-cache--update-parse)
                   and do (push it open-items)
                   and do (put-text-property
                           (1- (point)) (point) 'php7--pstate open-items)
                   else do (goto-char orig-match-end))

          (goto-char limit)
          (php7--ensure-cache--update-parse)
          (setq php7--cache-end limit)
          (setq php7--last-parse-pos limit)
          (setq php7--state-at-last-parse-pos open-items)
          )))))

(defun php7--end-of-defun-flat ()
  "Helper function for `php7-end-of-defun'."
  (cl-loop while (php7--re-search-forward "}" nil t)
           do (php7--ensure-cache)
           if (get-text-property (1- (point)) 'php7--pend)
           if (eq 'function (php7--pitem-type it))
           return t
           finally do (goto-char (point-max))))

(defun php7--end-of-defun-nested ()
  "Helper function for `php7-end-of-defun'."
  (message "test")
  (let* (pitem
         (this-end (save-excursion
                     (and (setq pitem (php7--beginning-of-defun-nested))
                          (php7--pitem-goto-h-end pitem)
                          (progn (backward-char)
                                 (forward-list)
                                 (point)))))
         found)

    (if (and this-end (< (point) this-end))
        ;; We're already inside a function; just go to its end.
        (goto-char this-end)

      ;; Otherwise, go to the end of the next function...
      (while (and (php7--re-search-forward "\\_<function\\_>" nil t)
                  (not (setq found (progn
                                     (goto-char (match-beginning 0))
                                     (php7--forward-function-decl))))))

      (if found (forward-list)
        ;; ... or eob.
        (goto-char (point-max))))))

(defun php7-end-of-defun (&optional arg)
  "Value of `end-of-defun-function' for `php7-mode'."
  (setq arg (or arg 1))
  (while (and (not (bobp)) (< arg 0))
    (cl-incf arg)
    (php7-beginning-of-defun)
    (php7-beginning-of-defun)
    (unless (bobp)
      (php7-end-of-defun)))

  (while (> arg 0)
    (cl-decf arg)
    ;; look for function backward. if we're inside it, go to that
    ;; function's end. otherwise, search for the next function's end and
    ;; go there
    (if php7-flat-functions
        (php7--end-of-defun-flat)

      ;; if we're doing nested functions, see whether we're in the
      ;; prologue. If we are, go to the end of the function; otherwise,
      ;; call php7--end-of-defun-nested to do the real work
      (let ((prologue-begin (php7--function-prologue-beginning)))
        (cond ((and prologue-begin (<= prologue-begin (point)))
               (goto-char prologue-begin)
               (re-search-forward "\\_<function")
               (goto-char (match-beginning 0))
               (php7--forward-function-decl)
               (forward-list))

              (t (php7--end-of-defun-nested)))))))

(defun php7--beginning-of-macro (&optional lim)
  (let ((here (point)))
    (save-restriction
      (if lim (narrow-to-region lim (point-max)))
      (beginning-of-line)
      (while (eq (char-before (1- (point))) ?\\)
        (forward-line -1))
      (back-to-indentation)
      (if (and (<= (point) here)
               (looking-at php7--opt-cpp-start))
          t
        (goto-char here)
        nil))))

(defun php7--backward-syntactic-ws (&optional lim)
  "Simple implementation of `c-backward-syntactic-ws' for `php7-mode'."
  (save-restriction
    (when lim (narrow-to-region lim (point-max)))

    (let ((in-macro (save-excursion (php7--beginning-of-macro)))
          (pos (point)))

      (while (progn (unless in-macro (php7--beginning-of-macro))
                    (forward-comment most-negative-fixnum)
                    (/= (point)
                        (prog1
                            pos
                          (setq pos (point)))))))))

(defun php7--forward-syntactic-ws (&optional lim)
  "Simple implementation of `c-forward-syntactic-ws' for `php7-mode'."
  (save-restriction
    (when lim (narrow-to-region (point-min) lim))
    (let ((pos (point)))
      (while (progn
               (forward-comment most-positive-fixnum)
               (when (eq (char-after) ?#)
                 (c-end-of-macro))
               (/= (point)
                   (prog1
                       pos
                     (setq pos (point)))))))))

;; Like (up-list -1), but only considers lists that end nearby"
(defun php7--up-nearby-list ()
  (save-restriction
    ;; Look at a very small region so our computation time doesn't
    ;; explode in pathological cases.
    (narrow-to-region (max (point-min) (- (point) 500)) (point))
    (up-list -1)))

(defun php7--inside-param-list-p ()
  "Return non-nil if point is in a function parameter list."
  (ignore-errors
    (save-excursion
      (php7--up-nearby-list)
      (and (looking-at "(")
           (progn (forward-symbol -1)
                  (or (looking-at "function")
                      (progn (forward-symbol -1)
                             (looking-at "function"))))))))

;;; Font Lock

(defvar php7--tmp-location nil)
(make-variable-buffer-local 'php7--tmp-location)

(defun php7--forward-destructuring-spec (&optional func)
  "Move forward over a PHP destructuring spec.
If FUNC is supplied, call it with no arguments before every
variable name in the spec.  Return true if this was actually a
spec.  FUNC must preserve the match data."
  (pcase (char-after)
    (?\[
     (forward-char)
     (while
         (progn
           (forward-comment most-positive-fixnum)
           (cond ((memq (char-after) '(?\[ ?\{))
                  (php7--forward-destructuring-spec func))

                 ((eq (char-after) ?,)
                  (forward-char)
                  t)

                 ((looking-at php7--name-re)
                  (and func (funcall func))
                  (goto-char (match-end 0))
                  t))))
     (when (eq (char-after) ?\])
       (forward-char)
       t))

    (?\{
     (forward-char)
     (forward-comment most-positive-fixnum)
     (while
         (when (looking-at php7--objfield-re)
           (goto-char (match-end 0))
           (forward-comment most-positive-fixnum)
           (and (cond ((memq (char-after) '(?\[ ?\{))
                       (php7--forward-destructuring-spec func))
                      ((looking-at php7--name-re)
                       (and func (funcall func))
                       (goto-char (match-end 0))
                       t))
                (progn (forward-comment most-positive-fixnum)
                       (when (eq (char-after) ?\,)
                         (forward-char)
                         (forward-comment most-positive-fixnum)
                         t)))))
     (when (eq (char-after) ?\})
       (forward-char)
       t))))

(defun php7--variable-decl-matcher (limit)
  "Font-lock matcher for variable names in a variable declaration.
This is a cc-mode-style matcher that *always* fails, from the
point of view of font-lock.  It applies highlighting directly with
`font-lock-apply-highlight'."
  (condition-case nil
      (save-restriction
        (narrow-to-region (point-min) limit)

        (let ((first t))
          (forward-comment most-positive-fixnum)
          (while
              (and (or first
                       (when (eq (char-after) ?,)
                         (forward-char)
                         (forward-comment most-positive-fixnum)
                         t))
                   (cond ((looking-at php7--name-re)
                          (font-lock-apply-highlight
                           '(0 font-lock-variable-name-face))
                          (goto-char (match-end 0)))

                         ((save-excursion
                            (php7--forward-destructuring-spec))

                          (php7--forward-destructuring-spec
                           (lambda ()
                             (font-lock-apply-highlight
                              '(0 font-lock-variable-name-face)))))))

            (forward-comment most-positive-fixnum)
            (when (eq (char-after) ?=)
              (forward-char)
              (php7--forward-expression)
              (forward-comment most-positive-fixnum))

            (setq first nil))))

    ;; Conditions to handle
    (scan-error nil)
    (end-of-buffer nil))

  ;; Matcher always "fails"
  nil)

(defconst php7--font-lock-keywords-3
  `(
    ;; This goes before keywords-2 so it gets used preferentially
    ;; instead of the keywords in keywords-2. Don't use override
    ;; because that will override syntactic fontification too, which
    ;; will fontify commented-out directives as if they weren't
    ;; commented out.
    ,@cpp-font-lock-keywords ; from font-lock.el

    ,@php7--font-lock-keywords-2

    ("\\.\\(prototype\\)\\_>"
     (1 font-lock-constant-face))

    ;; Highlights class being declared, in parts
    (php7--class-decl-matcher
     ,(concat "\\(" php7--name-re "\\)\\(?:\\.\\|.*$\\)")
     (goto-char (match-beginning 1))
     nil
     (1 font-lock-type-face))

    ;; Highlights parent class, in parts, if available
    (php7--class-decl-matcher
     ,(concat "\\(" php7--name-re "\\)\\(?:\\.\\|.*$\\)")
     (if (match-beginning 2)
         (progn
           (setq php7--tmp-location (match-end 2))
           (goto-char php7--tmp-location)
           (insert "=")
           (goto-char (match-beginning 2)))
       (setq php7--tmp-location nil)
       (goto-char (point-at-eol)))
     (when php7--tmp-location
       (save-excursion
         (goto-char php7--tmp-location)
         (delete-char 1)))
     (1 font-lock-type-face))

    ;; Highlights parent class
    (php7--class-decl-matcher
     (2 font-lock-type-face nil t))

    ;; variable declarations
    ,(list
      (concat "\\_<\\(const\\|var\\|let\\)\\_>\\|" php7--basic-type-re)
      (list #'php7--variable-decl-matcher nil nil nil))

    ;; class instantiation
    ,(list
      (concat "\\_<new\\_>\\s-+\\(" php7--dotted-name-re "\\)")
      (list 1 'font-lock-type-face))

    ;; instanceof
    ,(list
      (concat "\\_<instanceof\\_>\\s-+\\(" php7--dotted-name-re "\\)")
      (list 1 'font-lock-type-face))

    ;; formal parameters
    ,(list
      (concat
       "\\_<function\\_>\\(\\s-+" php7--name-re "\\)?\\s-*(\\s-*"
       php7--name-start-re)
      (list (concat "\\(" php7--name-re "\\)\\(\\s-*).*\\)?")
            '(backward-char)
            '(end-of-line)
            '(1 font-lock-variable-name-face)))

    ;; continued formal parameter list
    ,(list
      (concat
       "^\\s-*" php7--name-re "\\s-*[,)]")
      (list php7--name-re
            '(if (save-excursion (backward-char)
                                 (php7--inside-param-list-p))
                 (forward-symbol -1)
               (end-of-line))
            '(end-of-line)
            '(0 font-lock-variable-name-face))))
  "Level three font lock for `php7-mode'.")

(defun php7--inside-pitem-p (pitem)
  "Return whether point is inside the given pitem's header or body."
  (php7--ensure-cache)
  (cl-assert (php7--pitem-h-begin pitem))
  (cl-assert (php7--pitem-paren-depth pitem))

  (and (> (point) (php7--pitem-h-begin pitem))
       (or (null (php7--pitem-b-end pitem))
           (> (php7--pitem-b-end pitem) (point)))))

(defun php7--parse-state-at-point ()
  "Parse the PHP program state at point.
Return a list of `php7--pitem' instances that apply to point, most
specific first.  In the worst case, the current toplevel instance
will be returned."
  (save-excursion
    (save-restriction
      (widen)
      (php7--ensure-cache)
      (let ((pstate (or (save-excursion
                          (php7--backward-pstate))
                        (list php7--initial-pitem))))

        ;; Loop until we either hit a pitem at BOB or pitem ends after
        ;; point (or at point if we're at eob)
        (cl-loop for pitem = (car pstate)
                 until (or (eq (php7--pitem-type pitem)
                               'toplevel)
                           (php7--inside-pitem-p pitem))
                 do (pop pstate))

        pstate))))

(defun php7--syntactic-context-from-pstate (pstate)
  "Return the PHP syntactic context corresponding to PSTATE."
  (let ((type (php7--pitem-type (car pstate))))
    (cond ((memq type '(function macro))
           type)
          ((consp type)
           'class)
          (t 'toplevel))))

(defun php7-syntactic-context ()
  "Return the PHP syntactic context at point.
When called interactively, also display a message with that
context."
  (interactive)
  (let* ((syntactic-context (php7--syntactic-context-from-pstate
                             (php7--parse-state-at-point))))

    (when (called-interactively-p 'interactive)
      (message "Syntactic context: %s" syntactic-context))

    syntactic-context))

(defun php7--class-decl-matcher (limit)
  "Font lock function used by `php7-mode'.
This performs fontification according to `php7--class-styles'."
  (cl-loop initially (php7--ensure-cache limit)
           while (re-search-forward php7--quick-match-re limit t)
           for orig-end = (match-end 0)
           do (goto-char (match-beginning 0))
           if (cl-loop for style in '() ; php7--class-styles
                       for decl-re = (plist-get style :class-decl)
                       if (and (memq (php7-syntactic-context)
                                     (plist-get style :contexts))
                               decl-re
                               (looking-at decl-re))
                       do (goto-char (match-end 0))
                       and return t)
           return t
           else do (goto-char orig-end)))

(defconst php7--font-lock-keywords
  '(php7--font-lock-keywords-3 php7--font-lock-keywords-1
                                   php7--font-lock-keywords-2
                                   php7--font-lock-keywords-3)
  "Font lock keywords for `php7-mode'.  See `font-lock-keywords'.")

(defun php7-font-lock-syntactic-face-function (state)
  "Return syntactic face given STATE."
  (if (nth 3 state)
      font-lock-string-face
    (if (save-excursion
          (goto-char (nth 8 state))
          (looking-at "/\\*\\*"))
        font-lock-doc-face
      font-lock-comment-face)))

(defconst php7--syntax-propertize-regexp-regexp
  (rx
   ;; Start of regexp.
   "/"
   (0+ (or
        ;; Match characters outside of a character class.
        (not (any ?\[ ?/ ?\\))
        ;; Match backslash quoted characters.
        (and "\\" not-newline)
        ;; Match character class.
        (and
         "["
         (0+ (or
              (not (any ?\] ?\\))
              (and "\\" not-newline)))
         "]")))
   (group (zero-or-one "/")))
  "Regular expression matching a JavaScript regexp literal.")

(defun php7-syntax-propertize-regexp (end)
  (let ((ppss (syntax-ppss)))
    (when (eq (nth 3 ppss) ?/)
      ;; A /.../ regexp.
      (goto-char (nth 8 ppss))
      (when (looking-at php7--syntax-propertize-regexp-regexp)
        ;; Don't touch text after END.
        (when (> end (match-end 1))
          (setq end (match-end 1)))
        (put-text-property (match-beginning 1) end
                           'syntax-table (string-to-syntax "\"/"))
        (goto-char end)))))

(defun php7-syntax-propertize (start end)
  ;; JavaScript allows immediate regular expression objects, written /.../.
  (goto-char start)
  (php7-syntax-propertize-regexp end)
  (funcall
   (syntax-propertize-rules
    ;; Distinguish /-division from /-regexp chars (and from /-comment-starter).
    ;; FIXME: Allow regexps after infix ops like + ...
    ;; https://developer.mozilla.org/en/JavaScript/Reference/Operators
    ;; We can probably just add +, -, <, >, %, ^, ~, ?, : at which
    ;; point I think only * and / would be missing which could also be added,
    ;; but need care to avoid affecting the // and */ comment markers.
    ("\\(?:^\\|[=([{,:;|&!]\\|\\_<return\\_>\\)\\(?:[ \t]\\)*\\(/\\)[^/*]"
     (1 (ignore
	 (forward-char -1)
         (when (or (not (memq (char-after (match-beginning 0)) '(?\s ?\t)))
                   ;; If the / is at the beginning of line, we have to check
                   ;; the end of the previous text.
                   (save-excursion
                     (goto-char (match-beginning 0))
                     (forward-comment (- (point)))
                     (memq (char-before)
                           (eval-when-compile (append "=({[,:;" '(nil))))))
           (put-text-property (match-beginning 1) (match-end 1)
                              'syntax-table (string-to-syntax "\"/"))
           (php7-syntax-propertize-regexp end)))))
    ("\\`\\(#\\)!" (1 "< b")))
   (point) end))

(defconst php7--prettify-symbols-alist
  '(("=>" . ?⇒)
    (">=" . ?≥)
    ("<=" . ?≤))
  "Alist of symbol prettifications for PHP.")

;;; Indentation

(defconst php7--possibly-braceless-keyword-re
  (php7--regexp-opt-symbol
   '("catch" "do" "else" "finally" "for" "if" "try" "while" "with"
     "each"))
  "Regexp matching keywords optionally followed by an opening brace.")

(defconst php7--declaration-keyword-re
  (regexp-opt '("var" "let" "const") 'words)
  "Regular expression matching variable declaration keywords.")

(defconst php7--indent-operator-re
  (concat "[-+*/%<>&^|?:.]\\([^-+*/.]\\|$\\)\\|!?=\\|"
          (php7--regexp-opt-symbol '("in" "instanceof")))
  "Regexp matching operators that affect indentation of continued expressions.")

(defun php7--looking-at-operator-p ()
  "Return non-nil if point is on a PHP operator, other than a comma."
  (save-match-data
    (and (looking-at php7--indent-operator-re)
         (or (not (eq (char-after) ?:))
             (save-excursion
               (php7--backward-syntactic-ws)
               (when (= (char-before) ?\)) (backward-list))
               (and (php7--re-search-backward "[?:{]\\|\\_<case\\_>" nil t)
                    (eq (char-after) ??))))
         (not (and
               (eq (char-after) ?/)
               (save-excursion
                 (eq (nth 3 (syntax-ppss)) ?/))))
         (not (and
               (eq (char-after) ?*)
               ;; Generator method (possibly using computed property).
               (looking-at (concat "\\* *\\(?:\\[\\|" php7--name-re " *(\\)"))
               (save-excursion
                 (php7--backward-syntactic-ws)
                 ;; We might misindent some expressions that would
                 ;; return NaN anyway.  Shouldn't be a problem.
                 (memq (char-before) '(?, ?} ?{))))))))

(defun php7--find-newline-backward ()
  "Move backward to the nearest newline that is not in a block comment."
  (let ((continue t)
        (result t))
    (while continue
      (setq continue nil)
      (if (search-backward "\n" nil t)
          (let ((parse (syntax-ppss)))
            ;; We match the end of a // comment but not a newline in a
            ;; block comment.
            (when (nth 4 parse)
              (goto-char (nth 8 parse))
              ;; If we saw a block comment, keep trying.
              (unless (nth 7 parse)
                (setq continue t))))
        (setq result nil)))
    result))

(defun php7--continued-expression-p ()
  "Return non-nil if the current line continues an expression."
  (save-excursion
    (back-to-indentation)
    (if (php7--looking-at-operator-p)
        (or (not (memq (char-after) '(?- ?+)))
            (progn
              (forward-comment (- (point)))
              (not (memq (char-before) '(?, ?\[ ?\()))))
      (and (php7--find-newline-backward)
           (progn
             (skip-chars-backward " \t")
             (or (bobp) (backward-char))
             (and (> (point) (point-min))
                  (save-excursion (backward-char) (not (looking-at "[/*]/")))
                  (php7--looking-at-operator-p)
                  (and (progn (backward-char)
                              (not (looking-at "+\\+\\|--\\|/[/*]"))))))))))

(defun php7--skip-term-backward ()
  "Skip a term before point; return t if a term was skipped."
  (let ((term-skipped nil))
    ;; Skip backward over balanced parens.
    (let ((progress t))
      (while progress
        (setq progress nil)
        ;; First skip whitespace.
        (skip-syntax-backward " ")
        ;; Now if we're looking at closing paren, skip to the opener.
        ;; This doesn't strictly follow JS syntax, in that we might
        ;; skip something nonsensical like "()[]{}", but it is enough
        ;; if it works ok for valid input.
        (when (memq (char-before) '(?\] ?\) ?\}))
          (setq progress t term-skipped t)
          (backward-list))))
    ;; Maybe skip over a symbol.
    (let ((save-point (point)))
      (if (and (< (skip-syntax-backward "w_") 0)
                 (looking-at php7--name-re))
          ;; Skipped.
          (progn
            (setq term-skipped t)
            (skip-syntax-backward " "))
        ;; Did not skip, so restore point.
        (goto-char save-point)))
    (when (and term-skipped (> (point) (point-min)))
      (backward-char)
      (eq (char-after) ?.))))

(defun php7--skip-terms-backward ()
  "Skip any number of terms backward.
Move point to the earliest \".\" without changing paren levels.
Returns t if successful, nil if no term was found."
  (when (php7--skip-term-backward)
    ;; Found at least one.
    (let ((last-point (point)))
      (while (php7--skip-term-backward)
        (setq last-point (point)))
      (goto-char last-point)
      t)))

(defun php7--chained-expression-p ()
  "A helper for php7--proper-indentation that handles chained expressions.
A chained expression is when the current line starts with '.' and the
previous line also has a '.' expression.
This function returns the indentation for the current line if it is
a chained expression line; otherwise nil.
This should only be called while point is at the start of the line's content,
as determined by `back-to-indentation'."
  (when php7-chain-indent
    (save-excursion
      (when (and (eq (char-after) ?.)
                 (php7--continued-expression-p)
                 (php7--find-newline-backward)
                 (php7--skip-terms-backward))
        (current-column)))))

(defun php7--end-of-do-while-loop-p ()
  "Return non-nil if point is on the \"while\" of a do-while statement.
Otherwise, return nil.  A braceless do-while statement spanning
several lines requires that the start of the loop is indented to
the same column as the current line."
  (interactive)
  (save-excursion
    (save-match-data
      (when (looking-at "\\s-*\\_<while\\_>")
	(if (save-excursion
	      (skip-chars-backward "[ \t\n]*}")
	      (looking-at "[ \t\n]*}"))
	    (save-excursion
	      (backward-list) (forward-symbol -1) (looking-at "\\_<do\\_>"))
	  (php7--re-search-backward "\\_<do\\_>" (point-at-bol) t)
	  (or (looking-at "\\_<do\\_>")
	      (let ((saved-indent (current-indentation)))
		(while (and (php7--re-search-backward "^\\s-*\\_<" nil t)
			    (/= (current-indentation) saved-indent)))
		(and (looking-at "\\s-*\\_<do\\_>")
		     (not (php7--re-search-forward
			   "\\_<while\\_>" (point-at-eol) t))
		     (= (current-indentation) saved-indent)))))))))


(defun php7--ctrl-statement-indentation ()
  "Helper function for `php7--proper-indentation'.
Return the proper indentation of the current line if it starts
the body of a control statement without braces; otherwise, return
nil."
  (save-excursion
    (back-to-indentation)
    (when (save-excursion
            (and (not (eq (point-at-bol) (point-min)))
                 (not (looking-at "[{]"))
                 (php7--re-search-backward "[[:graph:]]" nil t)
                 (progn
                   (or (eobp) (forward-char))
                   (when (= (char-before) ?\)) (backward-list))
                   (skip-syntax-backward " ")
                   (skip-syntax-backward "w_")
                   (looking-at php7--possibly-braceless-keyword-re))
                 (memq (char-before) '(?\s ?\t ?\n ?\}))
                 (not (php7--end-of-do-while-loop-p))))
      (save-excursion
        (goto-char (match-beginning 0))
        (+ (current-indentation) php7-indent-level)))))

(defun php7--get-c-offset (symbol anchor)
  (let ((c-offsets-alist
         (list (cons 'c php7-comment-lineup-func))))
    (c-get-syntactic-indentation (list (cons symbol anchor)))))

(defun php7--same-line (pos)
  (and (>= pos (point-at-bol))
       (<= pos (point-at-eol))))

(defun php7--multi-line-declaration-indentation ()
  "Helper function for `php7--proper-indentation'.
Return the proper indentation of the current line if it belongs to a declaration
statement spanning multiple lines; otherwise, return nil."
  (let (forward-sexp-function ; Use Lisp version.
        at-opening-bracket)
    (save-excursion
      (back-to-indentation)
      (when (not (looking-at php7--declaration-keyword-re))
        (when (looking-at php7--indent-operator-re)
          (goto-char (match-end 0)))
        (while (and (not at-opening-bracket)
                    (not (bobp))
                    (let ((pos (point)))
                      (save-excursion
                        (php7--backward-syntactic-ws)
                        (or (eq (char-before) ?,)
                            (and (not (eq (char-before) ?\;))
                                 (prog2
                                     (skip-syntax-backward ".")
                                     (looking-at php7--indent-operator-re)
                                   (php7--backward-syntactic-ws))
                                 (not (eq (char-before) ?\;)))
                            (php7--same-line pos)))))
          (condition-case nil
              (backward-sexp)
            (scan-error (setq at-opening-bracket t))))
        (when (looking-at php7--declaration-keyword-re)
          (goto-char (match-end 0))
          (1+ (current-column)))))))

(defun php7--indent-in-array-comp (bracket)
  "Return non-nil if we think we're in an array comprehension.
In particular, return the buffer position of the first `for' kwd."
  (let ((end (point)))
    (save-excursion
      (goto-char bracket)
      (when (looking-at "\\[")
        (forward-char 1)
        (php7--forward-syntactic-ws)
        (if (looking-at "[[{]")
            (let (forward-sexp-function) ; Use Lisp version.
              (condition-case nil
                  (progn
                    (forward-sexp)       ; Skip destructuring form.
                    (php7--forward-syntactic-ws)
                    (if (and (/= (char-after) ?,) ; Regular array.
                             (looking-at "for"))
                        (match-beginning 0)))
                (scan-error
                 ;; Nothing to do here.
                 nil)))
          ;; To skip arbitrary expressions we need the parser,
          ;; so we'll just guess at it.
          (if (and (> end (point)) ; Not empty literal.
                   (re-search-forward "[^,]]* \\(for\\_>\\)" end t)
                   ;; Not inside comment or string literal.
                   (let ((status (parse-partial-sexp bracket (point))))
                     (and (= 1 (car status))
                          (not (nth 8 status)))))
              (match-beginning 1)))))))

(defun php7--array-comp-indentation (bracket for-kwd)
  (if (php7--same-line for-kwd)
      ;; First continuation line.
      (save-excursion
        (goto-char bracket)
        (forward-char 1)
        (skip-chars-forward " \t")
        (current-column))
    (save-excursion
      (goto-char for-kwd)
      (current-column))))

(defun php7--maybe-goto-declaration-keyword-end (parse-status)
  "Helper function for `php7--proper-indentation'.
Depending on the value of `php7-indent-first-init', move
point to the end of a variable declaration keyword so that
indentation is aligned to that column."
  (cond
   ((eq php7-indent-first-init t)
    (when (looking-at php7--declaration-keyword-re)
      (goto-char (1+ (match-end 0)))))
   ((eq php7-indent-first-init 'dynamic)
    (let ((bracket (nth 1 parse-status))
          declaration-keyword-end
          at-closing-bracket-p
          forward-sexp-function ; Use Lisp version.
          comma-p)
      (when (looking-at php7--declaration-keyword-re)
        (setq declaration-keyword-end (match-end 0))
        (save-excursion
          (goto-char bracket)
          (setq at-closing-bracket-p
                (condition-case nil
                    (progn
                      (forward-sexp)
                      t)
                  (error nil)))
          (when at-closing-bracket-p
            (while (forward-comment 1))
            (setq comma-p (looking-at-p ","))))
        (when comma-p
          (goto-char (1+ declaration-keyword-end))))))))

(defun php7--proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (cond ((nth 4 parse-status)    ; inside comment
           (php7--get-c-offset 'c (nth 8 parse-status)))
          ((nth 3 parse-status) 0) ; inside string
          ((eq (char-after) ?#) 0)
          ((save-excursion (php7--beginning-of-macro)) 4)
          ;; Indent array comprehension continuation lines specially.
          ((let ((bracket (nth 1 parse-status))
                 beg)
             (and bracket
                  (not (php7--same-line bracket))
                  (setq beg (php7--indent-in-array-comp bracket))
                  ;; At or after the first loop?
                  (>= (point) beg)
                  (php7--array-comp-indentation bracket beg))))
          ((php7--chained-expression-p))
          ((php7--ctrl-statement-indentation))
          ((php7--multi-line-declaration-indentation))
          ((nth 1 parse-status)
	   ;; A single closing paren/bracket should be indented at the
	   ;; same level as the opening statement. Same goes for
	   ;; "case" and "default".
           (let ((same-indent-p (looking-at "[]})]"))
                 (switch-keyword-p (looking-at "default\\_>\\|case\\_>[^:]"))
                 (continued-expr-p (php7--continued-expression-p)))
             (goto-char (nth 1 parse-status)) ; go to the opening char
             (if (or (not php7-indent-align-list-continuation)
                     (looking-at "[({[]\\s-*\\(/[/*]\\|$\\)"))
                 (progn ; nothing following the opening paren/bracket
                   (skip-syntax-backward " ")
                   (when (eq (char-before) ?\)) (backward-list))
                   (back-to-indentation)
                   (php7--maybe-goto-declaration-keyword-end parse-status)
                   (let* ((in-switch-p (unless same-indent-p
                                         (looking-at "\\_<switch\\_>")))
                          (same-indent-p (or same-indent-p
                                             (and switch-keyword-p
                                                  in-switch-p)))
                          (indent
                           (cond (same-indent-p
                                  (current-column))
                                 (continued-expr-p
                                  (+ (current-column) (* 2 php7-indent-level)
                                     php7-expr-indent-offset))
                                 (t
                                  (+ (current-column) php7-indent-level
                                     (pcase (char-after (nth 1 parse-status))
                                       (?\( php7-paren-indent-offset)
                                       (?\[ php7-square-indent-offset)
                                       (?\{ php7-curly-indent-offset)))))))
                     (if in-switch-p
                         (+ indent php7-switch-indent-offset)
                       indent)))
               ;; If there is something following the opening
               ;; paren/bracket, everything else should be indented at
               ;; the same level.
               (unless same-indent-p
                 (forward-char)
                 (skip-chars-forward " \t"))
               (current-column))))

          ((php7--continued-expression-p)
           (+ php7-indent-level php7-expr-indent-offset))
          (t (prog-first-column)))))

;;; JSX Indentation

(defsubst php7--jsx-find-before-tag ()
  "Find where JSX starts.

Assume JSX appears in the following instances:
- Inside parentheses, when returned or as the first argument
  to a function, and after a newline
- When assigned to variables or object properties, but only
  on a single line
- As the N+1th argument to a function

This is an optimized version of (re-search-backward \"[(,]\n\"
nil t), except set point to the end of the match.  This logic
executes up to the number of lines in the file, so it should be
really fast to reduce that impact."
  (let (pos)
    (while (and (> (point) (point-min))
                (not (progn
                       (end-of-line 0)
                       (when (or (eq (char-before) 40)   ; (
                                 (eq (char-before) 44))  ; ,
                         (setq pos (1- (point))))))))
    pos))

(defconst php7--jsx-end-tag-re
  (concat "</" sgml-name-re ">\\|/>")
  "Find the end of a JSX element.")

(defconst php7--jsx-after-tag-re "[),]"
  "Find where JSX ends.
This complements the assumption of where JSX appears from
`php7--jsx-before-tag-re', which see.")

(defun php7--jsx-indented-element-p ()
  "Determine if/how the current line should be indented as JSX.

Return `first' for the first JSXElement on its own line.
Return `nth' for subsequent lines of the first JSXElement.
Return `expression' for an embedded JS expression.
Return `after' for anything after the last JSXElement.
Return nil for non-JSX lines.

Currently, JSX indentation supports the following styles:

- Single-line elements (indented like normal JS):

  var element = <div></div>;

- Multi-line elements (enclosed in parentheses):

  function () {
    return (
      <div>
        <div></div>
      </div>
    );
 }

- Function arguments:

  React.render(
    <div></div>,
    document.querySelector('.root')
  );"
  (let ((current-pos (point))
        (current-line (line-number-at-pos))
        last-pos
        before-tag-pos before-tag-line
        tag-start-pos tag-start-line
        tag-end-pos tag-end-line
        after-tag-line
        parens paren type)
    (save-excursion
      (and
       ;; Determine if we're inside a jsx element
       (progn
         (end-of-line)
         (while (and (not tag-start-pos)
                     (setq last-pos (php7--jsx-find-before-tag)))
           (while (forward-comment 1))
           (when (= (char-after) 60) ; <
             (setq before-tag-pos last-pos
                   tag-start-pos (point)))
           (goto-char last-pos))
         tag-start-pos)
       (progn
         (setq before-tag-line (line-number-at-pos before-tag-pos)
               tag-start-line (line-number-at-pos tag-start-pos))
         (and
          ;; A "before" line which also starts an element begins with js, so
          ;; indent it like js
          (> current-line before-tag-line)
          ;; Only indent the jsx lines like jsx
          (>= current-line tag-start-line)))
       (cond
        ;; Analyze bounds if there are any
        ((progn
           (while (and (not tag-end-pos)
                       (setq last-pos (re-search-forward php7--jsx-end-tag-re nil t)))
             (while (forward-comment 1))
             (when (looking-at php7--jsx-after-tag-re)
               (setq tag-end-pos last-pos)))
           tag-end-pos)
         (setq tag-end-line (line-number-at-pos tag-end-pos)
               after-tag-line (line-number-at-pos after-tag-line))
         (or (and
              ;; Ensure we're actually within the bounds of the jsx
              (<= current-line tag-end-line)
              ;; An "after" line which does not end an element begins with
              ;; js, so indent it like js
              (<= current-line after-tag-line))
             (and
              ;; Handle another case where there could be e.g. comments after
              ;; the element
              (> current-line tag-end-line)
              (< current-line after-tag-line)
              (setq type 'after))))
        ;; They may not be any bounds (yet)
        (t))
       ;; Check if we're inside an embedded multi-line js expression
       (cond
        ((not type)
         (goto-char current-pos)
         (end-of-line)
         (setq parens (nth 9 (syntax-ppss)))
         (while (and parens (not type))
           (setq paren (car parens))
           (cond
            ((and (>= paren tag-start-pos)
                  ;; Curly bracket indicates the start of an embedded expression
                  (= (char-after paren) 123) ; {
                  ;; The first line of the expression is indented like sgml
                  (> current-line (line-number-at-pos paren))
                  ;; Check if within a closing curly bracket (if any)
                  ;; (exclusive, as the closing bracket is indented like sgml)
                  (cond
                   ((progn
                      (goto-char paren)
                      (ignore-errors (let (forward-sexp-function)
                                       (forward-sexp))))
                    (< current-line (line-number-at-pos)))
                   (t)))
             ;; Indicate this guy will be indented specially
             (setq type 'expression))
            (t (setq parens (cdr parens)))))
         t)
        (t))
       (cond
        (type)
        ;; Indent the first jsx thing like js so we can indent future jsx things
        ;; like sgml relative to the first thing
        ((= current-line tag-start-line) 'first)
        ('nth))))))

(defmacro php7--as-sgml (&rest body)
  "Execute BODY as if in sgml-mode."
  `(with-syntax-table sgml-mode-syntax-table
     (let (forward-sexp-function
           parse-sexp-lookup-properties)
       ,@body)))

(defun php7--expression-in-sgml-indent-line ()
  "Indent the current line as PHP or SGML (whichever is farther)."
  (let* (indent-col
         (savep (point))
         ;; Don't whine about errors/warnings when we're indenting.
         ;; This has to be set before calling parse-partial-sexp below.
         (inhibit-point-motion-hooks t)
         (parse-status (save-excursion
                         (syntax-ppss (point-at-bol)))))
    ;; Don't touch multiline strings.
    (unless (nth 3 parse-status)
      (setq indent-col (save-excursion
                         (back-to-indentation)
                         (if (>= (point) savep) (setq savep nil))
                         (php7--as-sgml (sgml-calculate-indent))))
      (if (null indent-col)
          'noindent
        ;; Use whichever indentation column is greater, such that the sgml
        ;; column is effectively a minimum
        (setq indent-col (max (php7--proper-indentation parse-status)
                              (+ indent-col php7-indent-level)))
        (if savep
            (save-excursion (indent-line-to indent-col))
          (indent-line-to indent-col))))))

(defun php7-indent-line ()
  "Indent the current line as PHP."
  (interactive)
  (let* ((parse-status
          (save-excursion (syntax-ppss (point-at-bol))))
         (offset (- (point) (save-excursion (back-to-indentation) (point)))))
    (unless (nth 3 parse-status)
      (indent-line-to (php7--proper-indentation parse-status))
      (when (> offset 0) (forward-char offset)))))

(defun php7-jsx-indent-line ()
  "Indent the current line as JSX (with SGML offsets).
i.e., customize JSX element indentation with `sgml-basic-offset',
`sgml-attribute-offset' et al."
  (interactive)
  (let ((indentation-type (php7--jsx-indented-element-p)))
    (cond
     ((eq indentation-type 'expression)
      (php7--expression-in-sgml-indent-line))
     ((or (eq indentation-type 'first)
          (eq indentation-type 'after))
      ;; Don't treat this first thing as a continued expression (often a "<" or
      ;; ">" causes this misinterpretation)
      (cl-letf (((symbol-function #'php7--continued-expression-p) 'ignore))
        (php7-indent-line)))
     ((eq indentation-type 'nth)
      (php7--as-sgml (sgml-indent-line)))
     (t (php7-indent-line)))))

;;; Filling

(defvar php7--filling-paragraph nil)

;; FIXME: Such redefinitions are bad style.  We should try and use some other
;; way to get the same result.
(defadvice c-forward-sws (around php7-fill-paragraph activate)
  (if php7--filling-paragraph
      (setq ad-return-value (php7--forward-syntactic-ws (ad-get-arg 0)))
    ad-do-it))

(defadvice c-backward-sws (around php7-fill-paragraph activate)
  (if php7--filling-paragraph
      (setq ad-return-value (php7--backward-syntactic-ws (ad-get-arg 0)))
    ad-do-it))

(defadvice c-beginning-of-macro (around php7-fill-paragraph activate)
  (if php7--filling-paragraph
      (setq ad-return-value (php7--beginning-of-macro (ad-get-arg 0)))
    ad-do-it))

(defun php7-c-fill-paragraph (&optional justify)
  "Fill the paragraph with `c-fill-paragraph'."
  (interactive "*P")
  (let ((php7--filling-paragraph t)
        (fill-paragraph-function #'c-fill-paragraph))
    (c-fill-paragraph justify)))

(defun php7-do-auto-fill ()
  (let ((php7--filling-paragraph t))
    (c-do-auto-fill)))

;;; Type database and Imenu

;; We maintain a cache of semantic information, i.e., the classes and
;; functions we've encountered so far. In order to avoid having to
;; re-parse the buffer on every change, we cache the parse state at
;; each interesting point in the buffer. Each parse state is a
;; modified copy of the previous one, or in the case of the first
;; parse state, the empty state.
;;
;; The parse state itself is just a stack of php7--pitem
;; instances. It starts off containing one element that is never
;; closed, that is initially php7--initial-pitem.
;;


(defun php7--pitem-format (pitem)
  (let ((name (php7--pitem-name pitem))
        (type (php7--pitem-type pitem)))

    (format "name:%S type:%S"
            name
            (if (atom type)
                type
              (plist-get type :name)))))

(defun php7--make-merged-item (item child name-parts)
  "Helper function for `php7--splice-into-items'.
Return a new item that is the result of merging CHILD into
ITEM.  NAME-PARTS is a list of parts of the name of CHILD
that we haven't consumed yet."
  (php7--debug "php7--make-merged-item: {%s} into {%s}"
                   (php7--pitem-format child)
                   (php7--pitem-format item))

  ;; If the item we're merging into isn't a class, make it into one
  (unless (consp (php7--pitem-type item))
    (php7--debug "php7--make-merged-item: changing dest into class")
    (setq item (make-php7--pitem
                :children (list item)

                ;; Use the child's class-style if it's available
                :type (if (atom (php7--pitem-type child))
                          php7--dummy-class-style
                  (php7--pitem-type child))

                :name (php7--pitem-strname item))))

  ;; Now we can merge either a function or a class into a class
  (cons (cond
         ((cdr name-parts)
          (php7--debug "php7--make-merged-item: recursing")
          ;; if we have more name-parts to go before we get to the
          ;; bottom of the class hierarchy, call the merger
          ;; recursively
          (php7--splice-into-items (car item) child
                                       (cdr name-parts)))

         ((atom (php7--pitem-type child))
          (php7--debug "php7--make-merged-item: straight merge")
          ;; Not merging a class, but something else, so just prepend
          ;; it
          (cons child (car item)))

         (t
          ;; Otherwise, merge the new child's items into those
          ;; of the new class
          (php7--debug "php7--make-merged-item: merging class contents")
          (append (car child) (car item))))
        (cdr item)))

(defun php7--pitem-strname (pitem)
  "Last part of the name of PITEM, as a string or symbol."
  (let ((name (php7--pitem-name pitem)))
    (if (consp name)
        (car (last name))
      name)))

(defun php7--splice-into-items (items child name-parts)
  "Splice CHILD into the `php7--pitem' ITEMS at NAME-PARTS.
If a class doesn't exist in the tree, create it.  Return
the new items list.  NAME-PARTS is a list of strings given
the broken-down class name of the item to insert."

  (let ((top-name (car name-parts))
        (item-ptr items)
        new-items last-new-item new-cons)

    (php7--debug "php7--splice-into-items: name-parts: %S items:%S"
             name-parts
             (mapcar #'php7--pitem-name items))

    (cl-assert (stringp top-name))
    (cl-assert (> (length top-name) 0))

    ;; If top-name isn't found in items, then we build a copy of items
    ;; and throw it away. But that's okay, since most of the time, we
    ;; *will* find an instance.

    (while (and item-ptr
                (cond ((equal (php7--pitem-strname (car item-ptr)) top-name)
                       ;; Okay, we found an entry with the right name. Splice
                       ;; the merged item into the list...
                       (setq new-cons (cons (php7--make-merged-item
                                             (car item-ptr) child
                                             name-parts)
                                            (cdr item-ptr)))

                       (if last-new-item
                           (setcdr last-new-item new-cons)
                         (setq new-items new-cons))

                       ;; ...and terminate the loop
                       nil)

                      (t
                       ;; Otherwise, copy the current cons and move onto the
                       ;; text. This is tricky; we keep track of the tail of
                       ;; the list that begins with new-items in
                       ;; last-new-item.
                       (setq new-cons (cons (car item-ptr) nil))
                       (if last-new-item
                           (setcdr last-new-item new-cons)
                         (setq new-items new-cons))
                       (setq last-new-item new-cons)

                       ;; Go to the next cell in items
                       (setq item-ptr (cdr item-ptr))))))

    (if item-ptr
        ;; Yay! We stopped because we found something, not because
        ;; we ran out of items to search. Just return the new
        ;; list.
        (progn
          (php7--debug "search succeeded: %S" name-parts)
          new-items)

      ;; We didn't find anything. If the child is a class and we don't
      ;; have any classes to drill down into, just push that class;
      ;; otherwise, make a fake class and carry on.
      (php7--debug "search failed: %S" name-parts)
      (cons (if (cdr name-parts)
                ;; We have name-parts left to process. Make a fake
                ;; class for this particular part...
                (make-php7--pitem
                 ;; ...and recursively digest the rest of the name
                 :children (php7--splice-into-items
                            nil child (cdr name-parts))
                 :type php7--dummy-class-style
                 :name top-name)

              ;; Otherwise, this is the only name we have, so stick
              ;; the item on the front of the list
              child)
            items))))

(defun php7--pitem-add-child (pitem child)
  "Copy `php7--pitem' PITEM, and push CHILD onto its list of children."
  (cl-assert (integerp (php7--pitem-h-begin child)))
  (cl-assert (if (consp (php7--pitem-name child))
              (cl-loop for part in (php7--pitem-name child)
                       always (stringp part))
            t))

  ;; This trick works because we know (based on our defstructs) that
  ;; the child list is always the first element, and so the second
  ;; element and beyond can be shared when we make our "copy".
  (cons

   (let ((name (php7--pitem-name child))
         (type (php7--pitem-type child)))

     (cond ((cdr-safe name) ; true if a list of at least two elements
            ;; Use slow path because we need class lookup
            (php7--splice-into-items (car pitem) child name))

           ((and (consp type)
                 (plist-get type :prototype))

            ;; Use slow path because we need class merging. We know
            ;; name is a list here because down in
            ;; `php7--ensure-cache', we made sure to only add
            ;; class entries with lists for :name
            (cl-assert (consp name))
            (php7--splice-into-items (car pitem) child name))

           (t
            ;; Fast path
            (cons child (car pitem)))))

   (cdr pitem)))

(defun php7--maybe-make-marker (location)
  "Return a marker for LOCATION if `imenu-use-markers' is non-nil."
  (if imenu-use-markers
      (set-marker (make-marker) location)
    location))

(defun php7--pitems-to-imenu (pitems unknown-ctr)
  "Convert PITEMS, a list of `php7--pitem' structures, to imenu format."

  (let (imenu-items pitem pitem-type pitem-name subitems)

    (while (setq pitem (pop pitems))
      (setq pitem-type (php7--pitem-type pitem))
      (setq pitem-name (php7--pitem-strname pitem))
      (when (eq pitem-name t)
        (setq pitem-name (format "[unknown %s]"
                                 (cl-incf (car unknown-ctr)))))

      (cond
       ((memq pitem-type '(function macro))
        (cl-assert (integerp (php7--pitem-h-begin pitem)))
        (push (cons pitem-name
                    (php7--maybe-make-marker
                     (php7--pitem-h-begin pitem)))
              imenu-items))

       ((consp pitem-type) ; class definition
        (setq subitems (php7--pitems-to-imenu
                        (php7--pitem-children pitem)
                        unknown-ctr))
        (cond (subitems
               (push (cons pitem-name subitems)
                     imenu-items))

              ((php7--pitem-h-begin pitem)
               (cl-assert (integerp (php7--pitem-h-begin pitem)))
               (setq subitems (list
                               (cons "[empty]"
                                     (php7--maybe-make-marker
                                      (php7--pitem-h-begin pitem)))))
               (push (cons pitem-name subitems)
                     imenu-items))))

       (t (error "Unknown item type: %S" pitem-type))))

    imenu-items))

(defun php7--imenu-create-index ()
  "Return an imenu index for the current buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (php7--ensure-cache)
      (cl-assert (or (= (point-min) (point-max))
                  (eq php7--last-parse-pos (point))))
      (when php7--last-parse-pos
        (let ((state php7--state-at-last-parse-pos)
              (unknown-ctr (cons -1 nil)))

          ;; Make sure everything is closed
          (while (cdr state)
            (setq state
                  (cons (php7--pitem-add-child (cl-second state) (car state))
                        (cddr state))))

          (cl-assert (= (length state) 1))

          ;; Convert the new-finalized state into what imenu expects
          (php7--pitems-to-imenu
           (car (php7--pitem-children state))
           unknown-ctr))))))

;; Silence the compiler.
(defvar which-func-imenu-joiner-function)

(defun php7--which-func-joiner (parts)
  (mapconcat #'identity parts "."))

(defun php7--imenu-to-flat (items prefix symbols)
  (cl-loop for item in items
           if (imenu--subalist-p item)
           do (php7--imenu-to-flat
               (cdr item) (concat prefix (car item) ".")
               symbols)
           else
           do (let* ((name (concat prefix (car item)))
                     (name2 name)
                     (ctr 0))

                (while (gethash name2 symbols)
                  (setq name2 (format "%s<%d>" name (cl-incf ctr))))

                (puthash name2 (cdr item) symbols))))

(defun php7--get-all-known-symbols ()
  "Return a hash table of all PHP symbols.
This searches all existing `php7-mode' buffers. Each key is the
name of a symbol (possibly disambiguated with <N>, where N > 1),
and each value is a marker giving the location of that symbol."
  (cl-loop with symbols = (make-hash-table :test 'equal)
           with imenu-use-markers = t
           for buffer being the buffers
           for imenu-index = (with-current-buffer buffer
                               (when (derived-mode-p 'php7-mode)
                                 (php7--imenu-create-index)))
           do (php7--imenu-to-flat imenu-index "" symbols)
           finally return symbols))

(defvar php7--symbol-history nil
  "History of entered PHP symbols.")

(defun php7--read-symbol (symbols-table prompt &optional initial-input)
  "Helper function for `php7-find-symbol'.
Read a symbol from SYMBOLS-TABLE, which is a hash table like the
one from `php7--get-all-known-symbols', using prompt PROMPT and
initial input INITIAL-INPUT.  Return a cons of (SYMBOL-NAME
. LOCATION), where SYMBOL-NAME is a string and LOCATION is a
marker."
  (unless ido-mode
    (ido-mode 1)
    (ido-mode -1))

  (let ((choice (ido-completing-read
                 prompt
                 (cl-loop for key being the hash-keys of symbols-table
                          collect key)
                 nil t initial-input 'php7--symbol-history)))
    (cons choice (gethash choice symbols-table))))

(defun php7--guess-symbol-at-point ()
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (save-excursion
        (goto-char (car bounds))
        (when (eq (char-before) ?.)
          (backward-char)
          (setf (car bounds) (point))))
      (buffer-substring (car bounds) (cdr bounds)))))

(defvar find-tag-marker-ring)           ; etags

;; etags loads ring.
(declare-function ring-insert "ring" (ring item))

(defun php7-find-symbol (&optional arg)
  "Read a PHP symbol and jump to it.
With a prefix argument, restrict symbols to those from the
current buffer.  Pushes a mark onto the tag ring just like
`find-tag'."
  (interactive "P")
  (require 'etags)
  (let (symbols marker)
    (if (not arg)
        (setq symbols (php7--get-all-known-symbols))
      (setq symbols (make-hash-table :test 'equal))
      (php7--imenu-to-flat (php7--imenu-create-index)
                               "" symbols))

    (setq marker (cdr (php7--read-symbol
                       symbols "Jump to: "
                       (php7--guess-symbol-at-point))))

    (ring-insert find-tag-marker-ring (point-marker))
    (switch-to-buffer (marker-buffer marker))
    (push-mark)
    (goto-char marker)))

;;; Main Function

;;;###autoload
(define-derived-mode php7-mode prog-mode "Php7"
  "Major mode for editing PHP7."
  :group 'php7-mode
  (setq-local indent-line-function #'php7-indent-line)
  (setq-local beginning-of-defun-function #'php7-beginning-of-defun)
  (setq-local end-of-defun-function #'php7-end-of-defun)
  (setq-local open-paren-in-column-0-is-defun-start nil)
  (setq-local font-lock-defaults
              (list php7--font-lock-keywords nil nil nil nil
                    '(font-lock-syntactic-face-function
                      . php7-font-lock-syntactic-face-function)))
  (setq-local syntax-propertize-function #'php7-syntax-propertize)
  (setq-local prettify-symbols-alist php7--prettify-symbols-alist)

  (setq-local parse-sexp-ignore-comments t)
  (setq-local parse-sexp-lookup-properties t)
  (setq-local which-func-imenu-joiner-function #'php7--which-func-joiner)

  ;; Comments
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local fill-paragraph-function #'php7-c-fill-paragraph)
  (setq-local normal-auto-fill-function #'php7-do-auto-fill)

  ;; Parse cache
  (add-hook 'before-change-functions #'php7--flush-caches t t)

  ;; Frameworks
  (php7--update-quick-match-re)

  ;; Imenu
  (setq imenu-case-fold-search nil)
  (setq imenu-create-index-function #'php7--imenu-create-index)

  ;; for filling, pretend we're cc-mode
  (setq c-comment-prefix-regexp "//+\\|\\**"
        c-paragraph-start "\\(@[[:alpha:]]+\\>\\|$\\)"
        c-paragraph-separate "$"
        c-block-comment-prefix "* "
        c-line-comment-starter "//"
        c-comment-start-regexp "/[*/]\\|\\s!"
        comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  (setq-local comment-line-break-function #'c-indent-new-comment-line)
  (setq-local c-block-comment-start-regexp "/\\*")
  (setq-local comment-multi-line t)

  (setq-local electric-indent-chars
	      (append "{}():;," electric-indent-chars)) ;FIXME: js2-mode adds "[]*".
  (setq-local electric-layout-rules
	      '((?\; . after) (?\{ . after) (?\} . before)))

  (let ((c-buffer-is-cc-mode t))
    ;; FIXME: These are normally set by `c-basic-common-init'.  Should
    ;; we call it instead?  (Bug#6071)
    (make-local-variable 'paragraph-start)
    (make-local-variable 'paragraph-separate)
    (make-local-variable 'paragraph-ignore-fill-prefix)
    (make-local-variable 'adaptive-fill-mode)
    (make-local-variable 'adaptive-fill-regexp)
    (c-setup-paragraph-variables))

  ;; Important to fontify the whole buffer syntactically! If we don't,
  ;; then we might have regular expression literals that aren't marked
  ;; as strings, which will screw up parse-partial-sexp, scan-lists,
  ;; etc. and produce maddening "unbalanced parenthesis" errors.
  ;; When we attempt to find the error and scroll to the portion of
  ;; the buffer containing the problem, JIT-lock will apply the
  ;; correct syntax to the regular expression literal and the problem
  ;; will mysteriously disappear.
  ;; FIXME: We should instead do this fontification lazily by adding
  ;; calls to syntax-propertize wherever it's really needed.
  ;;(syntax-propertize (point-max))
  )

;;;###autoload
(define-derived-mode php7-jsx-mode php7-mode "JSX"
  "Major mode for editing JSX.

To customize the indentation for this mode, set the SGML offset
variables (`sgml-basic-offset', `sgml-attribute-offset' et al.)
locally, like so:

  (defun set-jsx-indentation ()
    (setq-local sgml-basic-offset php7-indent-level))
  (add-hook \\='php7-jsx-mode-hook #\\='set-jsx-indentation)"
  :group 'php7-mode
  (setq-local indent-line-function #'php7-jsx-indent-line))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.php7?\\'" . php7-mode))

(eval-after-load 'folding
  '(when (fboundp 'folding-add-to-marks-list)
     (folding-add-to-marks-list 'php7-mode "// {{{" "// }}}" )))

;;;###autoload
(dolist (name (list "php" "php7"))
  (add-to-list 'interpreter-mode-alist (cons (purecopy name) 'php7-mode)))

(provide 'php7-mode)
;;; php7-mode.el ends here
