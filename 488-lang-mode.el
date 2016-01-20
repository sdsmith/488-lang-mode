;;;; 488-lang-mode.el -- major mode for the 488 Source Language
;; January 17, 2016
;;
;; Followed a combination of the following two guides
;; http://renormalist.net/Renormalist/EmacsLanguageModeCreationTutorial
;; http://ergoemacs.org/emacs/elisp_syntax_coloring.html




;;; HOOK
;; Define module hook2

(defvar 488-lang-mode-hook nil)



;;; KEYWORDS
;; Define keyword categories
(setq 488-lang-keywords '("and" "do" "else" "exit" "function" "if" "not" "or" "procedure" "repeat" "return" "then" "until" "var" "when" "while" "with"))
(setq 488-lang-types '("boolean" "integer"))
(setq 488-lang-constants '("false" "newline" "true"))
(setq 488-lang-functions '("read" "write"))

;; Generate regexp string for each category of keywords
(setq 488-lang-keywords-regexp (regexp-opt 488-lang-keywords t))
(setq 488-lang-types-regexp (regexp-opt 488-lang-types t))
(setq 488-lang-constants-regexp (regexp-opt 488-lang-keywords t))
(setq 488-lang-functions-regexp (regexp-opt 488-lang-functions t))

;; Create list for font-lock
(setq 488-lang-font-lock-keywords
      `(
        (,488-lang-keywords-regexp . font-lock-keyword-face)
        (,488-lang-types-regexp . font-lock-type-face)
        (,488-lang-constants-regexp . font-lock-constant-face)
        (,488-lang-functions-regexp . font-lock-function-name-face)))

;; Clear memory
(setq 488-lang-keywords nil)
(setq 488-lang-types nil)
(setq 488-lang-constants nil)
(setq 488-lang-functions nil)
(setq 488-lang-keywords-regexp nil)
(setq 488-lang-types-regexp nil)
(setq 488-lang-constants-regexp nil)
(setq 488-lang-functions-regexp nil)



;;; KEYMAP
;; Create keymap
(defvar 488-lang-mode-map
  (let ((488-lang-mode-map (make-keymap)))
    (define-key 488-lang-mode-map "\C-j" 'newline-and-indent)
    488-lang-mode-map)
  "Keymap for 488 Lang major mode")



;;; AUTOLOAD
(add-to-list 'auto-mode-alist '("\\.488\\'" . 488-lang-mode))



;;; INDENTATION
;; Rules:
;; 1 - if beginning of buffer, indent 0
;; 2 - if end of scope block, de0indent relative to previous line
;; 3 - if end of scope block before current line, indent our current line to same indentaton as end of scope block
;; 4 - if new scope keyword, increase indent relative to start of line
;; 5 - if above rules do no apply, do not indent
;; NOTE(sdsmith): additional rule - need to de indent when ending scope for if, or loops, when no '}'
(defun 488-lang-indent-line ()
  "Indent current line as 488 Lang code"
  (interactive) ; Allows use in 'M-x' (for debugging)
  (beginning-of-line) ; Set point to beginning of line
  (if (bobp) ; Rule 1
      (indent-line-to 0)
    (let ((not-indented t) cur-indent)
      (cond ((looking-at "^[ \t]*ELSE") ; Rule 2
              (progn
                (save-excursion
                  (forward-line -1)
                  (setq cur-indent (- (current-indentation) default-tab-width)))
                (if (< cur-indent 0) ; Sanity check - make sure indent >= 0
                    (setq cur-indent 0))))
            ((looking-at "^[ \t]*}")
             ;; Find the top of the scope, and indent it at that level
             (save-excursion
               (let ((not-top-scope-found t))
                 (while not-top-scope-found
                   (forward-line -1)
                   (if (looking-at "^[ \t]*{")
                       (progn
                         (setq cur-ident (current-indentation))
                         (setq not-top-scope-found nil)))))))
            ;; Iterate backward through code to find indentation 'hint'
            (t
             (save-excursion
               (while not-indented
                 (forward-line -1)
                 (if (looking-at "^[ \t]*}") ; Rule 3
                     (progn
                       (setq cur-indent (current-indentation))
                       (setq not-indented nil))
                   ;; New scope keywords
                   ;; (regexp-opt '("IF" "WHILE" "REPEAT" "FUNCTION" "PROCEDURE" "{") t)
                   (if (looking-at "^[ \t]*\\(IF\\|ELSE\\|REPEAT\\|WHILE\\|{\\)") ; Rule 4
                       (progn
                         (setq cur-indent (+ (current-indentation) default-tab-width))
                         (setq not-indented nil))
                     (if (bobp) ; Rule 5
                         (setq not-indented nil))))))))
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0))))) ; If we didn't see indentation hint, allow not indentation



;;; SYNTAX TABLE
(defvar 488-lang-mode-syntax-table
  (let ((488-lang-mode-syntax-table (make-syntax-table)))

    ;; Make '_' a valid part of a word
    (modify-syntax-entry ?_ "w" 488-lang-mode-syntax-table)

    ;; Make '%' a comment until the end of a line
    (modify-syntax-entry ?% "<" 488-lang-mode-syntax-table) 
    (modify-syntax-entry ?\n ">" 488-lang-mode-syntax-table)

    ;; Open parathenesis character
    (modify-syntax-entry ?\( "(" 488-lang-mode-syntax-table)
    (modify-syntax-entry ?\) ")" 488-lang-mode-syntax-table)
    (modify-syntax-entry ?\[ "(" 488-lang-mode-syntax-table)
    (modify-syntax-entry ?\] ")" 488-lang-mode-syntax-table)
    (modify-syntax-entry ?{ "(" 488-lang-mode-syntax-table)
    (modify-syntax-entry ?} ")" 488-lang-mode-syntax-table)
    (modify-syntax-entry ?\" "\"" 488-lang-mode-syntax-table)

    ;; Add expression prefixes
    (modify-syntax-entry ?: "'" 488-lang-mode-syntax-table)

    ;;(modify-syntax-entry ? "" 488-lang-mode-syntax-table)
    488-lang-mode-syntax-table)
  "Syntax table for 488-lang-mode")



;; ;;; ENTRY FUNCTION
;; ;; Function to be called by emacs when mode is started
;; (defun 488-lang-mode ()
;;   "Major mode for CSC488 Source Language"
;;   (interactive)
;;   (kill-all-local-variables)
;;   (set-syntax-table 488-lang-mode-syntax-table) ; Set syntax table
;;   (use-local-map 488-lang-mode-map) ; Set key map
;;   (set (make-local-variable 'font-lock-defaults) '(488-lang-font-lock-keywords)) ; Set font lock
;;   ;(setq font-lock-defaults '((488-lang-font-lock-keywords)))
;;   (set (make-local-variable 'indent-line-function) '488-lang-indent-line) ; Set indent function
;;   (setq major-mode '488-lang-mode) ; Set major mode
;;   (setq mode-name "488 Lang") ; Set name to appear in buffer
;;   (run-hooks '488-lang-mode-hook))
    
(define-derived-mode 488-lang-mode fundamental-mode
  "488 Lang mode"
  "Major mode for CSC488 Source Language (2016)"
  (setq font-lock-defaults '((488-lang-font-lock-keywords))))


;;; EXPOSE MODULE
;; Add mode to 'features' list (ie. exposing it to the emacs environment)
(provide '488-lang-mode)

