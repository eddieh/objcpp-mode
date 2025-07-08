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
  :group 'prog-mode)

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
  t `(
      ;; Unary.
      (prefix "++" "--" "+" "-" "!" "~"
	      ,@(when (c-major-mode-is 'objc++-mode)
		  '("@selector" "@protocol" "@encode")))))

(c-lang-defconst c-other-op-syntax-tokens
  objc++ (append '("#" "##"		; Used by cpp.
		   "+" "-")
		 (c-lang-const c-other-op-syntax-tokens)))

(c-lang-defconst c-primitive-type-kwds
  objc++ (append
	  '("id" "Class" "SEL" "IMP" "BOOL")
	  (c-lang-const c-primitive-type-kwds)))

(c-lang-defconst c-class-decl-kwds
  objc++ '("struct" "union"
	   "@interface" "@implementation" "@protocol"))

;; (c-lang-defconst c-other-block-decl-kwds
;;     objc++ '("@interface" "@implementation" "@protocol"))

(c-lang-defconst c-modifier-kwds
  objc++ '("auto" "bycopy" "byref" "extern" "in" "inout" "oneway" "out" "static"))

(c-lang-defconst c-other-decl-kwds
  objc++ '("@class" "@defs" "@end" "@property" "@dynamic" "@synthesize"
	   "@compatibility_alias"))

(c-lang-defconst c-protection-kwds
  objc++ '("@private" "@protected" "@package" "@public"
	   "@required" "@optional"))

(c-lang-defconst c-type-list-kwds
  objc++ '("@class"))

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

(c-lang-defconst c-type-decl-end-used
  t (or (c-lang-const c-recognize-colon-labels)
	(and (c-lang-const c-label-kwds) t))
  ;; `c-decl-end' is used to mark the end of the @-style directives in
  ;; Objective-C.
  objc++ t)
(c-lang-defvar c-type-decl-end-used (c-lang-const c-type-decl-end-used))

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
    (beginning-of-line)
    (c-save-buffer-state
	((indent-point (point)))

      (cond

       ;; ObjC Method
       ((save-excursion
	  (goto-char (c-point 'boi))
	  (and c-opt-method-key
	       (looking-at c-opt-method-key)
	       (save-excursion
		 (c-beginning-of-statement-1 (c-determine-limit 1000) t))))
	`((objc-method-intro ,(c-point 'boi))))

       ;; ;; @interface, @implementation, @protocol
       ;; ((save-excursion
       ;; 	  (c-beginning-of-statement-1)
       ;; 	  (c-at-toplevel-p))
       ;; 	`((topmost-intro ,(c-point 'boi))))

       ((save-excursion
	  (catch 'not-in-directive
	    (c-beginning-of-statement-1)
	    (setq placeholder (point))
	    (while (and (c-forward-objc-directive)
			(< (point) indent-point))
	      (c-forward-syntactic-ws)
	      (if (>= (point) indent-point)
		  (throw 'not-in-directive t))
	      (setq placeholder (point)))
	    nil))
	(goto-char placeholder)
	`((topmost-intro ,(c-point 'boi))))

       (t
	(apply orig-fun args))))))

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
    map)
  "Keymap used in `objc++-mode' buffers.")

(easy-menu-define objc++-mode-menu objc++-mode-map "ObjC++ Mode Commands."
  (cons "ObjC++" (c-lang-const c-mode-menu objc++)))

;;;###autoload
(define-derived-mode objc++-mode prog-mode "ObjC++"
  "Major mode for editing ObjC++ code.

Key bindings:
\\{objc++-mode-map}"
  :after-hook (c-update-modeline)
  (c-initialize-cc-mode t)
  (c-init-language-vars objc++-mode)
  (c-common-init 'objc++-mode)
  ;;(setq-local c-doc-comment-style '((objc++-mode . ???)))
  (run-mode-hooks 'c-mode-common-hook))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc++-mode))

(provide 'objc++-mode)
