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


(defun objc++-unsyntacticp ()
  "Checks if point is in a string or comment."
  (c-in-literal nil t))

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

  (let ((start (point))
	objc-defun-found defun-found
	objc-defun-pos defun-pos)

    (setq objc-defun-found (objc++-beginning-of-defun-1)
	  objc-defun-pos (point))

    (goto-char start)

    (setq defun-found (c-beginning-of-defun)
	  defun-pos (point))

    (if (and objc-defun-found defun-found)
	(if (< (- start objc-defun-pos) (- start defun-pos))
	    (goto-char objc-defun-pos))
      (if objc-defun-found
	  (goto-char objc-defun-pos)
	defun-found))))

;; move backwards to ObjC defun, if not found return nil
(defun objc++-beginning-of-defun-1 ()
  (let ((start (point))
	found)
    (while (and (not (bobp))
		(not found))
      ;; use regexp search backwards unbounded & always move
      (if (re-search-backward (c-lang-const c-opt-class-key) nil 'move)
	  (cond
	   ((objc++-unsyntacticp)
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
  (let ((start (point))
	objc-defun-found defun-found
	objc-defun-pos defun-pos)

    (setq objc-defun-found (objc++-end-of-defun-1)
	  objc-defun-pos (point))

    (goto-char start)

    (setq defun-found (c-end-of-defun)
	  defun-pos (point))

    (if (and objc-defun-found defun-found)
	(if (< (- objc-defun-pos start) (- defun-pos start))
	    (goto-char objc-defun-pos))
      (if objc-defun-found
	  (goto-char objc-defun-pos)
	defun-found))))

;; move forward to ObjC defun, if not found return nil
(defun objc++-end-of-defun-1 ()
  (c-syntactic-re-search-forward "@end" nil t))

(defun objc++-beginning-of-statement ()
  (interactive)
  ;; cc-mode's interactive `c-beginning-of-statement' has a bug and
  ;; the -1 variant does not
  (catch 'done
    (while t
      (c-beginning-of-statement-1)
      (unless (or (eq (char-before) ?\() (eq (char-before) ?<)) (throw 'done nil))
      (unless (c-go-up-list-backward) (throw 'done nil)))))

(defun objc++-end-of-statement ()
  (interactive)
  (c-end-of-statement))

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
	 found-syntax
	 stmt-beg maybe-stmt-end
	 objc-defun-beg objc-defun-end objc-defun-kwd
	 defun-beg defun-end defun-kwd)
      (save-excursion

	(save-excursion
	  (unless found-syntax
	    (goto-char (c-point 'boi))
	    (setq found-syntax
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

		   ))))

	(save-excursion
	  (unless found-syntax
	    (objc++-beginning-of-statement)
	    (setq stmt-beg (point))
	    (setq found-syntax
		  (cond

		   ;; @interface ClassName
		   ((looking-at (c-lang-const c-opt-class-key))
		    (save-excursion
		      (objc++-forward-directive-1)
		      (setq maybe-stmt-end (point))
		      (or (when (< start maybe-stmt-end)
			    ;; class declaration continued
			    `((objc-class-cont ,(c-point 'boi))))
			  ;; inside class
			  `(;(inclass ,(c-point 'boi))
			    (topmost-intro ,(c-point 'bol))))))

		   ;; ;; @public, @private, etc
		   ;; ((looking-at (c-lang-const c-opt-protection-key))
		   ;;  `((access-label ,(c-point 'boi))
		   ;;    (topmost-intro ,(c-point 'bol))))

		   ;; ;; ObjC method
		   ;; ((looking-at (c-lang-const c-opt-method-key))
		   ;;  `((objc-method-intro ,(c-point 'boi))
		   ;;    (topmost-intro ,(c-point 'bol))))

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

		   ))))

      (message "found-syntax %S" found-syntax)

      (if (not found-syntax)
	(save-excursion
	  (if (objc++-beginning-of-defun-1)
	      (setq objc-defun-beg (point)
		    objc-defun-kwd (and
				    (looking-at c-keywords-regexp)
				    (c-keyword-sym (match-string 1)))
		    objc-defun-end (and (objc++-end-of-defun-1) (point))))
	  ;; (message "objc:\n beg=%S\n end=%S\n kwd=%S"
	  ;; 	   objc-defun-beg objc-defun-end objc-defun-kwd)
	  (goto-char start)
	  (if (c-beginning-of-defun)
	      (setq defun-beg (point)
		    defun-kwd (and
			       (looking-at c-keywords-regexp)
			       (c-keyword-sym (match-string 1)))
		    defun-end (and (c-end-of-defun) (point))))
	  ;; (message "c++:\n beg=%S\n end=%S\n kwd=%S"
	  ;; 	   defun-beg defun-end defun-kwd)
	  (goto-char start)
	  (if (not objc-defun-beg)
	      (apply orig-fun args)
	    (if (not (and defun-beg (>= objc-defun-beg defun-beg)))
		(apply orig-fun args)
	      (cond

	       ;; NOTE: keeping this for now since we might want to
	       ;; tag elements with the inclass syntatic element
	       ;; symbol

	       (t
		(apply orig-fun args))))))
	found-syntax)))))

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
  (c-initialize-cc-mode t)
  (setq abbrev-mode t)
  (c-init-language-vars objc++-mode)
  (c-common-init 'objc++-mode)
  ;;(setq-local c-doc-comment-style '((objc++-mode . ???)))

  (setq-local c-macro-names-with-semicolon
	      '("NS_HEADER_AUDIT_BEGIN"
		"APPKIT_API_UNAVAILABLE_BEGIN_MACCATALYST"
		"NS_ASSUME_NONNULL_BEGIN"))

  (run-mode-hooks 'c-mode-common-hook))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc++-mode))

(provide 'objc++-mode)
