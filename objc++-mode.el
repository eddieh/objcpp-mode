;;; objc++-mode.el --- major-mode for Objective-C++ sources  -*- lexical-binding: t -*-

;; Author: 2025 Eddie Hillenbrand
;;
;; URL: https://github.com/eddieh/objc++-mode
;; Created: July 2025
;; Keywords: objc objc++ c++ languages oop
;; Version: 0.0.1
;; Package-Requires: ((emacs "30.1") (cc-mode 5.33))
;;
;; This file is NOT part of Emacs.
;;
;; This program is public domain.

;;; Commentary:

;; Major-mode for the Objective-C++ programming language.

;;; Code:

(require 'cc-mode)

(eval-when-compile
  (require 'cl)
  (require 'cc-langs)
  (require 'cc-fonts)
  (require 'cc-menus))


(defgroup objc++ nil
  "Major mode for editing ObjC++ code."
  :group 'prog-mode
  :prefix "objc++-")


(eval-and-compile
  (c-add-language 'objc++-mode 'c++-mode)

  (defun objc++--make-mode-syntax-table ()
    (let ((table (make-syntax-table)))
      (c-populate-syntax-table table)
      (modify-syntax-entry ?@ "_" table)
      table))
  (defvar objc++--make-mode-syntax-table #'objc++--make-mode-syntax-table
    "Workaround for Emacs bug#57065."))

(c-lang-defconst c-make-mode-syntax-table
  objc++ #'objc++--make-mode-syntax-table)

(c-lang-defconst c-identifier-syntax-modifications
  objc++ (append '((?@ . "w"))
                 (c-lang-const c-identifier-syntax-modifications)))

(c-lang-defconst c-symbol-start
  objc++ (concat "[" c-alpha "_@$]"))

(c-lang-defconst c-symbol-chars
  objc++ (concat c-alnum "_$@"))

(c-lang-defconst c-cpp-include-directives
  objc++ '("include" "import"))

(c-lang-defconst c-operators
  objc++ '("@selector" "@protocol" "@encode" "@available"))

(c-lang-defconst c-other-op-syntax-tokens
  objc++ (append '("#" "##"		; Used by cpp.
		   "+" "-")
		 (c-lang-const c-other-op-syntax-tokens)))

(c-lang-defconst c-literal-start-regexp
  ;; Regexp to match the start of comments and string literals.
  objc++ (concat (c-lang-const c-comment-start-regexp)
		 "\\|"
		 (if (memq 'gen-string-delim c-emacs-features)
		     "\"\\|\\s|@\"|"
		   "\"")))
(c-lang-defvar c-literal-start-regexp (c-lang-const c-literal-start-regexp))


(c-lang-defconst c-primitive-type-kwds
  objc++ (append
	  '("id" "Class" "SEL" "IMP" "BOOL")
	  (c-lang-const c-primitive-type-kwds)))

(c-lang-defconst c-class-decl-kwds
  objc++ '("@interface" "@implementation" "@protocol"))

;; (c-lang-defconst c-other-block-decl-kwds
;;     objc++ '("@interface" "@implementation" "@protocol"))

(c-lang-defconst c-modifier-kwds
  objc++ '("auto" "bycopy" "byref" "extern" "in"
	   "inout" "oneway" "out" "static"))

(c-lang-defconst c-other-decl-kwds
  objc++ '("@class" "@defs" "@end"
	   "@property" "@dynamic" "@synthesize"
	   "@compatibility_alias"))

(c-lang-defconst c-protection-kwds
  objc++ '("@private" "@protected" "@package" "@public"
	   "@required" "@optional"))

(c-lang-defconst c-type-list-kwds
  objc++ '("@class" "@protocol"))

;; @import is not actually a "list"
;;(c-lang-defconst c-ref-list-kwds
(c-lang-defconst c-typeless-decl-kwds
  objc++ '("@import"))

(c-lang-defconst c-paren-type-kwds
  objc++ '("@defs"))

(c-lang-defconst c-<>-type-kwds
  objc++ '("id"))

(c-lang-defconst c-block-stmt-1-kwds
  objc++ '("@finally" "@try" "@autoreleasepool"))

(c-lang-defconst c-block-stmt-2-kwds
  objc++ '("@catch" "@synchronized"))

(c-lang-defconst c-simple-stmt-kwds
  objc++ '("@throw"))

(c-lang-defconst c-constant-kwds
  objc++ '("nil" "Nil" "YES" "NO" "IBAction" "IBOutlet"
	   "NS_DURING" "NS_HANDLER" "NS_ENDHANDLER"))

(c-lang-defconst c-primary-expr-kwds
  objc++ '("super" "self"))

(c-lang-defconst c-decl-prefix-re
  objc++ (concat "\\([{}();,]+\\|"
		 (c-make-keywords-re nil (c-lang-const c-protection-kwds))
		 "\\)"))

(c-lang-defconst c-opt-extra-label-key
  objc++ (c-make-keywords-re t (c-lang-const c-protection-kwds)))

(c-lang-defconst c-opt-import-key
  objc++ (c-make-keywords-re t (c-lang-const c-ref-list-kwds)))
(c-lang-defvar c-opt-import-key (c-lang-const c-opt-import-key))

(c-lang-defconst c-opt-forward-decl-key
  objc++ (c-make-keywords-re t (c-lang-const c-type-list-kwds)))
(c-lang-defvar c-opt-forward-decl-key (c-lang-const c-opt-forward-decl-key))

(c-lang-defconst c-opt-class-key
  objc++ (c-make-keywords-re t (c-lang-const c-class-decl-kwds)))
(c-lang-defvar c-opt-class-key (c-lang-const c-opt-class-key))

(c-lang-defconst c-opt-protection-key
  objc++ (c-make-keywords-re t (c-lang-const c-protection-kwds)))
(c-lang-defvar c-opt-protection-key (c-lang-const c-opt-protection-key))

(c-lang-defconst c-opt-property-key
  objc++ (c-make-keywords-re t
	   '("@property" "@dynamic" "@synthesize")))
(c-lang-defvar c-opt-property-key (c-lang-const c-opt-property-key))

(c-lang-defconst c-opt-method-key
  objc++ (concat
	  ;; TODO: Ought to use a better method than anchoring on bol.
	  "^\\s *"
	  "\\([+-]\\)"
	  (c-lang-const c-simple-ws) "*"
	  (concat "\\("			; Return type.
		  "([^)]*)"
		  (c-lang-const c-simple-ws) "*"
		  "\\)?")
	  "\\(" (c-lang-const c-symbol-key) "\\)"))
(c-lang-defvar c-opt-method-key (c-lang-const c-opt-method-key))

(c-lang-defconst c-opt-protocol-op-key
  objc++ (concat
	  "@protocol" (c-lang-const c-simple-ws) "*" "\("))
(c-lang-defvar c-opt-protocol-op-key (c-lang-const c-opt-protocol-op-key))

(c-lang-defconst c-opt-class-forward-decl-key
  objc++ (concat
	  "@class"
	  (c-lang-const c-simple-ws) "+"
	  "\\(" (c-lang-const c-symbol-key) "\\)"
	  (c-lang-const c-simple-ws) "*"
	  "\\([,;]\\)"))
(c-lang-defvar c-opt-class-forward-decl-key
  (c-lang-const c-opt-class-forward-decl-key))

(c-lang-defconst c-opt-protocol-forward-decl-key
  objc++ (concat
	  "@protocol"
	  (c-lang-const c-simple-ws) "+"
	  "\\(" (c-lang-const c-symbol-key) "\\)"
	  (c-lang-const c-simple-ws) "*"
	  "\\([,;]\\)"))
(c-lang-defvar c-opt-protocol-forward-decl-key
  (c-lang-const c-opt-protocol-forward-decl-key))

(c-lang-defconst c-type-decl-end-used
  objc++ t)
(c-lang-defvar c-type-decl-end-used (c-lang-const c-type-decl-end-used))

(defun objc++-font-lock-put-face (beg end face)
  (if (featurep 'xemacs)
      (c-put-font-lock-face
       (1+ beg) (if end (1- end) (point)) font-lock-string-face)
    (c-put-font-lock-face
     beg (or end (point)) font-lock-string-face)))

;; This removes the warning face from import delimiters that
;; `c-font-lock-c++-modules' adds. In C++ import module statements
;; must be terminated with a semicolon, whereas in Objective-C/C++ the
;; #import directive should not be terminated with a semicolon.
;;
;; This should only remove the warning face from #import directives
;; while leaving import module statements alone.
(defun objc++-font-lock-fix-import (limit)
  (while (and (< (point) limit)
	      (re-search-forward
	       (concat (c-lang-const c-opt-cpp-prefix) "\\<\\(import\\)\\>")
	       limit t))
    (goto-char (match-end 1))
    (let (name-bounds pos beg end
		      module-names)
      (unless (c-skip-comments-and-strings limit)
	(save-excursion
	  (and (equal (match-string-no-properties 1) "import")
	       (< (point) limit)
	       (progn (c-forward-syntactic-ws limit)
		      (setq pos (point)))) ;;)
	  (goto-char pos)
	  (cond

	   ;; #import "header.h"
	   ((and (eq (char-after) ?\")
		 (setq pos (point))
		 (c-safe (c-forward-sexp) t))
	    (when (eq (char-before) ?\")
	      (setq beg pos
		    end (point)))
	    (objc++-font-lock-put-face beg end font-lock-string-face)
	    (c-forward-syntactic-ws limit)
	    t)

	   ;; #import <header>
	   ((and (looking-at "<\\(?:\\\\.\\|[^\\\n\r\t>]\\)*\\(>\\)?")
		 (< (match-end 0) limit))
	    (setq beg (point))
	    (goto-char (match-end 0))
	    (when (match-end 1)
	      (setq end (point)))
	    (objc++-font-lock-put-face beg end font-lock-string-face)
	    (c-forward-syntactic-ws limit)
	    t)

	   (t nil)))))))

(c-lang-defconst c-cpp-matchers
  objc++ (append
          ;; Merge with cc-mode defaults - enables us to add more
          ;; later
          (c-lang-const c-cpp-matchers)))

(c-lang-defconst c-basic-matchers-before
  objc++ (append

	  `(;; Fontify class names in the beginning of message
	    ;; expressions.
	    ,(c-make-font-lock-search-function
	      "\\["
	      '((c-fontify-types-and-refs ()
		  (c-forward-syntactic-ws limit)
		  (let ((start (point)))
		    ;; In this case we accept both primitive and known
		    ;; types.
		    (when (eq (c-forward-type) 'known)
		      (goto-char start)
		      (let ((c-promote-possible-types t))
			(c-forward-type))))
		  (if (> (point) limit) (goto-char limit)))))

	    ;; The @interface/@implementation/@protocol directives.
	    ,(c-make-font-lock-search-function
	      (concat "\\<"
		      (regexp-opt
		       '("@interface" "@implementation" "@protocol")
		       t)
		      "\\>")
	      '((c-fontify-types-and-refs
		    (;; The font-lock package in Emacs is known to
		     ;; clobber `parse-sexp-lookup-properties' (when
		     ;; it exists).
		     (parse-sexp-lookup-properties
		      (cc-eval-when-compile
			(boundp 'parse-sexp-lookup-properties))))
		  (objc++-forward-directive)
		  nil)
		(goto-char (match-beginning 0)))))

	  ;; Merge with cc-mode defaults - enables us to add more
	  ;; later
	  (c-lang-const c-basic-matchers-before)))

(c-lang-defconst c-simple-decl-matchers
  objc++ (append
	  ;; Merge with cc-mode defaults - enables us to add more
	  ;; later
	  (c-lang-const c-simple-decl-matchers)))

(c-lang-defconst c-complex-decl-matchers
  objc++ (append
	  ;; Merge with cc-mode defaults - enables us to add more
	  ;; later
	  (c-lang-const c-complex-decl-matchers)))

(c-lang-defconst c-basic-matchers-after
  objc++ (append

	  '(objc++-font-lock-fix-import)

	  ;; Merge with cc-mode defaults - enables us to add more
	  ;; later
	  (c-lang-const c-basic-matchers-after)))

(defun objc++-nearest-backward-fn (&rest fn-list)
  (let ((start (point))
	found pos pos-list nearest)
    (dolist (fn fn-list pos-list)
      (save-excursion
	(push `(,(if (funcall fn)
		     (- start (point))
		   (progn (beginning-of-buffer) (- start (point))))
		,(point) ,(if (symbolp fn) (symbol-name fn) "(anon)"))
	      pos-list)))
    ;;(message "%S" pos-list)
    (setq nearest (car pos-list))
    (dolist (n pos-list nearest)
      (if (< (car n) (car nearest))
	  (setq nearest n)))
    (goto-char (cadr nearest))))

(defun objc++-nearest-forward-fn (&rest fn-list)
  (let ((start (point))
	found pos pos-list nearest)
    (dolist (fn fn-list pos-list)
      (save-excursion
	(push `(,(if (funcall fn)
		     (- (point) start)
		   (progn (end-of-buffer) (- (point) start)))
		,(point) ,(if (symbolp fn) (symbol-name fn) "(anon)"))
	      pos-list)))
    ;;(message "%S" pos-list)
    (setq nearest (car pos-list))
    (dolist (n pos-list nearest)
      (if (< (car n) (car nearest))
	  (setq nearest n)))
    (goto-char (cadr nearest))))


(defun objc++-unsyntactic-p ()
  "Checks if point is in a string or comment."
  (c-in-literal nil t))

(defun objc++-in-property-def-p ()
  ;; Return nil if we aren't in a property definition, otherwise the
  ;; position of the initial @.
  ;;
  ;; This function might do hidden buffer changes.
  (save-excursion
    (back-to-indentation)
    (and c-opt-property-key
	 (looking-at c-opt-property-key)
	 (point))))

(defun objc++-in-method-def-p ()
  ;; Return nil if we aren't in a method definition, otherwise the
  ;; position of the initial [+-].
  ;;
  ;; This function might do hidden buffer changes.
  (save-excursion
    (beginning-of-line)
    (and c-opt-method-key
	 (looking-at c-opt-method-key)
	 (point))))


;; Objective-C & Objective-C++ add some defun-ish types.
;;
;; Class:
;;
;;     @interface ClassName
;;     @end
;;
;; Protocol:
;;
;;     @protocol ProtocolName
;;     @end
;;
;; Implementation:
;;
;;     @implementation ClassName
;;     @end
;;
;; Methods:
;;
;;     - (void)methodName {
;;     }
;;
;;     + newClassNameWithObject:(id)obj {
;;     }
;;
;; Literal array:
;;
;;     @[ obj1, obj2, obj3 ]
;;
;; Literal dictionaries:
;;
;;     @{ @"key1": obj1, @"key2": obj2 }
;;
;; Blocks:
;;
;;     ^{ ... }
;;
;;     ^ () { ... }
;;
;;     ^ void (void) { ... }
;;
;;     ^ (int arg1) { ... }
;;
;;     ^ int (int arg1) { return 42; }

(defun objc++-beginning-of-defun ()
  "Move backward to beginning of the closest ObjC defun or C/C++ defun."
  (interactive)
  (objc++-nearest-backward-fn
   #'objc++-beginning-of-defun-1
   #'c-beginning-of-defun))

;; move backwards to ObjC defun, if not found return nil
(defun objc++-beginning-of-defun-1 ()
  (let ((start (point))
	found)
    (while (and (not (bobp))
		(not found))
      ;; use regexp search backwards unbounded & always move
      (if (re-search-backward (c-lang-const c-opt-class-key) nil 'move)
	  (cond
	   ((objc++-unsyntactic-p)
	    (c-backward-syntactic-ws))
	   ((looking-at (c-lang-const c-opt-protocol-op-key)))
	   ((looking-at (c-lang-const c-opt-protocol-forward-decl-key)))
	   (t
	    (setq found t)))))
    (if (not found)
	(progn (goto-char start) nil)
      t)))

(defun objc++-end-of-defun ()
  "Move forward to end of the closest ObjC defun or C/C++ defun."
  (interactive)
  (objc++-nearest-forward-fn
   #'objc++-end-of-defun-1
   #'c-end-of-defun))

;; move forward to ObjC defun, if not found return nil
(defun objc++-end-of-defun-1 ()
  (c-syntactic-re-search-forward "@end" nil t))

(defun objc++-beginning-of-property ()
  (let ((start (point))
	found)
    (while (and (not (bobp))
		(not found))
      ;; use regexp search backwards unbounded & always move
      (if (re-search-backward (c-lang-const c-opt-property-key) nil 'move)
	  (cond
	   ((objc++-unsyntactic-p) (c-backward-syntactic-ws))
	   (t (setq found t)))))
    (if (not found)
	(progn (goto-char start) nil)
      t)))

(defun objc++-beginning-of-@end ()
  (let ((start (point))
	found)
    (while (and (not (bobp))
		(not found))
      ;; use regexp search backwards unbounded & always move
      (if (re-search-backward "@end" nil 'move)
	  (cond
	   ((objc++-unsyntactic-p) (c-backward-syntactic-ws))
	   (t (setq found t)))))
    (if (not found)
	(progn (goto-char start) nil)
      t)))

;; Need to handle property follows class-intro
;;
;;     @interface Object : Super
;;     @property String *prop;
;;     @end
;;
;;                            ^
;;
;; Need to handle inside a paren like part of a method or property
;; with spaces
;;
;;     - (void)method:(     String *)arg
;;
;;                    ^-------------^
;;
;;     @property (     readonly   ) String *property;
;;
;;               ^----------------^
;;
;; Need to handle @end as a statement when class-like is empty
;;
;;     @interface Object : Super
;;     @end
;;
;;         ^
(defun objc++-beginning-of-statement ()
  (interactive)

  (objc++-nearest-backward-fn
   #'objc++-beginning-of-defun-1
   #'objc++-beginning-of-property
   #'objc++-beginning-of-@end
   (lambda ()
     (let (start)
       (setq start (point))
       (catch 'done
	 (while (not (bobp))
	   (c-beginning-of-statement-1)
	   (when (and
		  (not (looking-at c-opt-method-key))
		  (or
		   (eq start (point))
		   (not (c-at-statement-start-p))))
	     (c-backward-syntactic-ws) t)
	   (unless (or
		    (eq (char-before) ?\()
		    (eq (char-before) ?<)
		    (eq (char-before) ?{))
	     (throw 'done t))
	   (unless (c-go-up-list-backward) (throw 'done t))))))))

(defun objc++-end-of-statement ()
  (interactive)
  (objc++-nearest-forward-fn
   #'objc++-end-of-defun-1
   (lambda ()
     (or (c-end-of-statement) t))))

(defconst objc++-simple-directive-key
  (eval-when-compile
    (c-make-keywords-re t
      (append
       (c-lang-const c-protection-kwds objc++) '("@end"))
      'objc++-mode)))

(defun objc++-forward-simple-directive ()
  "Move forward over class access directives, protocol @optional &
@required directives and @end."
  (and (looking-at objc++-simple-directive-key)
       (goto-char (match-end 1))))

(defun objc++-forward-directive ()
  (interactive)
  (objc++-forward-directive-1))

(defun objc++-forward-directive-1 ()
  ;; Assuming the point is at the beginning of a token, try to move
  ;; forward to the end of the Objective-C directive that starts
  ;; there.  Return t if a directive was fully recognized, otherwise
  ;; the point is moved as far as one could be successfully parsed and
  ;; nil is returned.
  ;;
  ;; This function records identifier ranges on
  ;; `c-record-type-identifiers' and `c-record-ref-identifiers' if
  ;; `c-record-type-identifiers' is non-nil.
  ;;
  ;; This function might do hidden buffer changes.

  (let ((start (point))
	start-char
	(c-promote-possible-types t)
	lim
	;; Turn off recognition of angle bracket arglists while parsing
	;; types here since the protocol reference list might then be
	;; considered part of the preceding name or superclass-name.
	c-recognize-<>-arglists)

    (if (or
	 (objc++-forward-simple-directive)

	 (and
	  (looking-at (c-lang-const c-opt-class-key))

	  ;; Fowward keyword and class-name
	  (progn
	    (goto-char (match-end 0))
	    (setq lim (point))
	    (c-skip-ws-forward)
	    (c-forward-type))

	  (catch 'break
	    ;; Look for ": superclass-name" or "( category-name )".
	    (when (looking-at "[:(]")
	      (setq start-char (char-after))
	      (forward-char)
	      (c-forward-syntactic-ws)
	      (unless (c-forward-type) (throw 'break nil))
	      (when (eq start-char ?\()
		(unless (eq (char-after) ?\)) (throw 'break nil))
		(forward-char)
		(c-forward-syntactic-ws)))

	    ;; Look for a < protocol-reference-list >
	    (if (eq (char-after) ?<)
		(let ((c-recognize-<>-arglists t)
		      (c-parse-and-markup-<>-arglists t)
		      c-restricted-<>-arglists)
		  (c-forward-<>-arglist t))
	      t))))

	(c-backward-syntactic-ws lim)

      ;; (progn
      ;;   (c-backward-syntactic-ws lim)
      ;;   (c-clear-c-type-property start (1- (point)) 'c-decl-end)
      ;;   (c-put-c-type-property (1- (point)) 'c-decl-end)
      ;;   t)

      ;; (c-clear-c-type-property start (point) 'c-decl-end)

      nil)))

(advice-add
 'c-forward-declarator
 :around #'objc++-forward-declarator-guard)

;; (advice-mapc (lambda (fn prop) (message "%s" fn)) 'c-forward-declarator)
;; (advice-remove 'c-forward-declarator #'objc++-forward-declarator-guard)

(defun objc++-forward-declarator-guard (orig-fun &rest args)
  (if (c-major-mode-is 'objc++-mode)
      (apply #'objc++-forward-declarator orig-fun args)
    (apply orig-fun args)))

(defun objc++-forward-declarator (orig-fun &rest args)
  (let ((here (point))
	(limit (car args))
	id-start id-end brackets-after-id paren-depth decorated
	got-init arglist double-double-quote pos)
    (or limit (setq limit (point-max)))
    (if (looking-at (c-lang-const c-opt-class-key))
	(let (found)
	  (prog1
	      (setq found
		    (c-syntactic-re-search-forward
		     ;; Consider making the next regexp a
		     ;; c-lang-defvar (2023-07-04).
		     (if (c-major-mode-is 'objc++-mode)
			 "\\(?:@end\\)\\|[;:,]\\|\\(=\\|[[(]\\)"
		       "[;:,]\\|\\(=\\|\\s(\\)")
		     limit 'limit t))
	    (setq got-init
		  (and found (match-beginning 1))))
	  (when (and found
		     (memq (char-before) '(?\; ?\: ?, ?= ?\( ?\[ ?{)))
	    (backward-char))
	  (list id-start id-end brackets-after-id got-init decorated arglist))
      (goto-char here)
      (apply orig-fun args))))

(advice-add
 'c-just-after-func-arglist-p
 :around #'objc++-just-after-func-arglist-p)

;; (advice-mapc (lambda (fn prop) (message "%s" fn)) 'c-just-after-func-arglist-p)
;; (advice-remove 'c-just-after-func-arglist-p #'objc++-just-after-func-arglist-p)

(defun objc++-just-after-func-arglist-p (orig-fun &rest args)
  (let ((lim (car args)))
    (if (c-major-mode-is 'objc++-mode)
	(and
	 (eq (c-beginning-of-statement-1 lim nil nil nil t) 'same)
	 (not (objc++-forward-directive-1)))
      (apply orig-fun args))))

(advice-add
 'c-guess-basic-syntax
 :around #'objc++-guess-basic-syntax-guard)

;; (advice-mapc (lambda (fn prop) (message "%s" fn)) 'c-guess-basic-syntax)
;; (advice-remove 'c-guess-basic-syntax #'objc++-guess-basic-syntax)

(defun objc++-guess-basic-syntax-guard (orig-fun &rest args)
  (if (c-major-mode-is 'objc++-mode)
      (apply #'objc++-guess-basic-syntax orig-fun args)
    (apply orig-fun args)))

(defun objc++-guess-basic-syntax (orig-fun &rest args)
  (save-excursion
    (c-save-buffer-state
	((start (point))
	 stmt-beg maybe-stmt-end maybe-next-stmt-beg)
      (or

       (save-excursion
	 (goto-char (c-point 'boi))
	 (cond

	  ;; @import AppKit;
	  ((looking-at (c-lang-const c-opt-import-key))
	   `((objc-import-intro ,(c-point 'boi))
	     (topmost-intro ,(c-point 'bol))))

	  ;; @class ForwardClassOne, ForwardClassTwo;
	  ((looking-at (c-lang-const c-opt-class-forward-decl-key))
	   `((objc-forward-decl-intro ,(c-point 'boi))
	     (topmost-intro ,(c-point 'bol))))

	  ;; @protocol ForwardProtoOne, ForwardProtoTwo;
	  ((looking-at (c-lang-const c-opt-protocol-forward-decl-key))
	   `((objc-forward-decl-intro ,(c-point 'boi))
	     (topmost-intro ,(c-point 'bol))))

	  ;; @interface ClassName
	  ((looking-at (c-lang-const c-opt-class-key))
	   `((objc-class-intro ,(c-point 'boi))
	     (topmost-intro ,(c-point 'bol))))

	  ;; @public, @private, etc
	  ((looking-at (c-lang-const c-opt-protection-key))
	   `((access-label ,(c-point 'boi))
	     (topmost-intro ,(c-point 'bol))))

	  ;; ObjC method
	  ((looking-at (c-lang-const c-opt-method-key))
	   `((objc-method-intro ,(c-point 'boi))
	     (topmost-intro ,(c-point 'bol))))

	  ;; @property
	  ((looking-at (c-lang-const c-opt-property-key))
	   `((objc-property-intro ,(c-point 'boi))
	     (topmost-intro ,(c-point 'bol))))

	  ;; @end
	  ((looking-at "@end")
	   `((objc-class-end ,(c-point 'boi))
	     (topmost-intro ,(c-point 'bol))))

	  ))

       (save-excursion
	 (objc++-beginning-of-statement)
	 (setq stmt-beg (point))
	 (cond

	  ;; @interface ClassName
	  ((looking-at (c-lang-const c-opt-class-key))
	   (save-excursion
	     (objc++-forward-directive-1)
	     (setq maybe-stmt-end (point))
	     (or (when (< start maybe-stmt-end)
		   ;; class declaration continued
		   `((objc-class-cont ,(c-point 'bol))))
		 ;; inside class
		 `(;(inclass ,(c-point 'boi))
		   (topmost-intro ,(c-point 'bol))))))

	  ;; ;; "@private" "@protected" "@package" "@public"
	  ;; ;; assume inside class def brace list { @public ... }
	  ;; ((looking-at (c-lang-const c-opt-protection-key))
	  ;;  `((access-label ,(c-point 'boi))
	  ;;    (topmost-intro ,(c-point 'bol))))
	  ;; ;; "@required" "@optional"
	  ;; ;; assume inside a protocol def

	  ;; ObjC method
	  ((looking-at (c-lang-const c-opt-method-key))
	   (save-excursion
	     (objc++-end-of-statement)
	     (setq maybe-stmt-end (point))

	     (or
	      (when (< start maybe-stmt-end)
		(objc++-beginning-of-statement)
		(setq maybe-next-stmt-beg (point))

		(or
		 (when (not (eq stmt-beg maybe-next-stmt-beg))
		   `((objc-method-args-cont ,(c-point 'bol))
		     (unterminated-method-decl 0)))

		 (when (< start maybe-stmt-end)
		   (save-excursion
		     (c-forward-syntactic-ws (c-point 'bonl))
		     (if (eq (char-after) ?{)
			 `((objc-method-args-cont ,(c-point 'bol))
			   (unterminated-method-def 0))
		       `((objc-method-args-cont ,(c-point 'bol))
			 (method-decl-or-def 0)))))))

	      (when (>= start maybe-stmt-end)
		(or
		 (save-excursion
		   (goto-char start)
		   (goto-char (c-point 'bol))
		   (c-forward-syntactic-ws (c-point 'boi))
		   (when (eq (char-after) ?{)
		     `((defun-open ,(c-point 'bol))
		       (medod-def 0))))

		 (save-excursion
		   (goto-char start)
		   (goto-char (c-point 'bol))
		   (when (c-syntactic-re-search-forward
			  "{" (c-point 'bonl) t)
		     `((objc-method-args-cont ,(c-point 'bol))
		       (method-def 1))))

		 (save-excursion
		   (c-end-of-statement)
		   (setq maybe-next-stmt-beg (point))
		   (goto-char start)
		   (if (<= (point) maybe-next-stmt-beg)
		       (if (c-syntactic-re-search-forward
			    "{" maybe-next-stmt-beg t)
			   (progn
			     (goto-char (c-point 'bol))
			     (if (< start (point))
				 `((objc-method-args-cont ,(c-point 'bol))
				   (method-def 2))))
			 (progn
			   (goto-char start)
			   (goto-char (c-point 'bol))
			   (if (c-syntactic-re-search-forward
				";" (c-point 'bonl) t)
			       `((objc-method-args-cont ,(c-point 'bol))
				 (terminated-method-decl 0))
			     `((topmost-intro ,(c-point 'bol))
			       (after-method-decl 0))))))))))))

	  ;; ;; @property
	  ;; ((looking-at (c-lang-const c-opt-property-key))
	  ;;  `((objc-property-intro ,(c-point 'boi))
	  ;;    (topmost-intro ,(c-point 'bol))))


	  ;; @end
	  ((looking-at "@end")
	   (save-excursion
	     (objc++-forward-directive-1)
	     (setq maybe-stmt-end (point))
	     (when (> start maybe-stmt-end)
	       ;; after @end
	       `((topmost-intro ,(c-point 'bol))))))

	  ))

       (apply orig-fun args)))))


(defcustom objc++-font-lock-extra-types nil
  (c-make-font-lock-extra-types-blurb
   "ObjC++" "objc++-mode"
   (concat
    "For example, a value of (\"[" c-upper "]\\\\sw*[" c-lower "]\\\\sw*\") means
capitalized words are treated as type names (the requirement for a
lower case char is to avoid recognizing all-caps macro and constant
names)."))
  :type 'c-extra-types-widget
  :safe #'c-string-list-p
  :group 'objc++)

(defconst objc++-font-lock-keywords-1 (c-lang-const c-matchers-1 objc++)
  "Minimal font locking for ObjC++ mode.")

(defconst objc++-font-lock-keywords-2 (c-lang-const c-matchers-2 objc++)
  "Fast normal font locking for ObjC++ mode.")

(defconst objc++-font-lock-keywords-3 (c-lang-const c-matchers-3 objc++)
  "Accurate normal font locking for ObjC++ mode.")

(defvar objc++-font-lock-keywords objc++-font-lock-keywords-3
  "Default expressions to highlight in ObjC++ mode.")

(defun objc++-font-lock-keywords-2 ()
  (c-compose-keywords-list objc++-font-lock-keywords-2))
(defun objc++-font-lock-keywords-3 ()
  (c-compose-keywords-list objc++-font-lock-keywords-3))
(defun objc++-font-lock-keywords ()
  (c-compose-keywords-list objc++-font-lock-keywords))


(defconst objc++-syle
  '((c-basic-offset . 4)
    (c-backslash-column . 78)
    (c-backslash-max-column . 78)
    (c-cleanup-list . (brace-else-brace
		       brace-elseif-brace
		       brace-catch-brace
                       empty-defun-braces
		       defun-close-semi
		       list-close-comma
		       scope-operator))
    ;; do not indent lines containing only start-of-comment more than
    ;; default
    (c-comment-only-line-offset . 0)
    ;; start new lines after braces
    ;; default is: before and after (for all other cases)
    (c-hanging-braces-alist . ((defun-open . (before after))
			       (defun-close . (before after))
			       (block-open . (after))
			       (block-close . c-snug-do-while)
			       (substatement-open . after)
			       (statement-case-open . nil)
			       (brace-list-open . after)
			       (brace-list-close . nil)
			       (class-open after)
                               (class-close before after)
                               (inexpr-class-open after)
                               (inexpr-class-close before)
                               (namespace-open after)
                               (inline-open after)
                               (inline-close before after)
			       (extern-lang-open after)
                               (extern-lang-close after)))
    ;; where to put newlines around colons
    (c-hanging-colons-alist . ((case-label)
                               (label after)
                               (access-label after)
                               (member-init-intro before)
                               (inher-intro)))
    (c-indent-comments-syntactically-p . t)
    ;; no spaces needed before a label
    ;; (c-label-minimum-indentation . 0)
    ;; define offsets for some code parts
    (c-offsets-alist . ((innamespace           . 0)
			(inextern-lang         . 0)
			(access-label          . -)
			(case-label            . 0)
			(member-init-intro     . +)
			(topmost-intro         . 0)
			(arglist-cont-nonempty . +)
			(block-open            . 0)
			(brace-list-open       . +)
			(brace-list-intro      . +)
			(brace-list-entry      . 0)
			(brace-list-close      . 0)
			(label                 . -)
			(statement-cont        . 4)
			(substatement-open     . 0)
			(case-label            . 0)
			(inher-intro           . ++)
			(inline-open           . 0)))
    ;; indent line when pressing tab, instead of a plain tab character
    (c-tab-always-indent . t)
    (indent-tabs-mode . nil)
    (tab-width . 4))
  "ObjC++ Style")

(c-add-style "objc++" objc++-syle)

(eval-and-compile
  (unless (or (stringp c-default-style)
              (assoc 'objc++-mode c-default-style))
    (setq c-default-style
          (cons '(objc++-mode . "objc++") c-default-style))))


(defvar objc++-mode-syntax-table
  (funcall (c-lang-const c-make-mode-syntax-table objc++))
  "Syntax table used in `objc++-mode' buffers.")

(defvar objc++-mode-map
  (let ((map (c-make-inherited-keymap)))

    ;; remap defun movement
    (define-key map [remap beginning-of-defun]
		#'objc++-beginning-of-defun)
    (define-key map [remap end-of-defun]
		#'objc++-end-of-defun)
    (define-key map [remap c-beginning-of-defun]
		#'objc++-beginning-of-defun)
    (define-key map [remap c-end-of-defun]
		#'objc++-end-of-defun)

    ;; rempap statement movement
    (define-key map [remap beginning-of-statement]
		#'objc++-beginning-of-statement)
    (define-key map [remap end-of-statement]
		#'objc++-end-of-statement)
    (define-key map [remap c-beginning-of-statement]
		#'objc++-beginning-of-statement)
    (define-key map [remap c-end-of-statement]
		#'objc++-end-of-statement)

    map)
  "Keymap used in `objc++-mode' buffers.")

(easy-menu-define objc++-mode-menu objc++-mode-map "ObjC++ Mode Commands."
  (cons "ObjC++" (c-lang-const c-mode-menu objc++)))

(defcustom objc++-mode-hook nil
  "Hook called by `objc++-mode'."
  :type 'hook
  :group 'objc++)

;;;###autoload
(define-derived-mode objc++-mode prog-mode "ObjC++"
  "Major mode for editing ObjC++ code.

Key bindings:
\\{objc++-mode-map}"
  :group 'objc++
  :after-hook (progn (c-make-noise-macro-regexps)
		     (c-make-macro-with-semi-re)
		     ;; (message "used for init debugging")
		     (c-update-modeline))

  ;; (setq-local debug-on-error t)

  (c-initialize-cc-mode t)
  (setq abbrev-mode t)
  (c-init-language-vars objc++-mode)
  (c-common-init 'objc++-mode)

  ;; (setq-local c-doc-comment-style '((objc++-mode . ???)))

  (setq-local c-macro-names-with-semicolon
	      '("NS_HEADER_AUDIT_BEGIN"
		"NS_HEADER_AUDIT_END"
		"APPKIT_API_UNAVAILABLE_BEGIN_MACCATALYST"
		"API_UNAVAILABLE_END"
		"NS_ASSUME_NONNULL_BEGIN"
		"NS_ASSUME_NONNULL_END"))

  (run-mode-hooks 'c-mode-common-hook))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc++-mode))

(provide 'objc++-mode)
