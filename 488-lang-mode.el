;;;; 488-lang-mode.el -- major mode for the 488 Source Language
;; January 17, 2016
;;
;; Followed a combination of the following two guides
;; http://renormalist.net/Renormalist/EmacsLanguageModeCreationTutorial
;; http://ergoemacs.org/emacs/elisp_syntax_coloring.html



(defun what-line-val ()
  "Return the current buffer line number and narrowed line number of point."
  (interactive)
  (let ((start (point-min))
	(n (line-number-at-pos)))
    (if (= start 1)
	n
      (save-excursion
	(save-restriction
	  (widen)
          (+ n (line-number-at-pos start) -1))))))



;;; HOOK
;; Define module hook
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



;;;###autoload
(add-to-list 'auto-mode-alist '("\\.488\\'" . 488-lang-mode))
      


;;; INDENTATION
(setq 488-lang--min-line-indent nil)
(setq 488-lang--max-line-indent nil)
(setq 488-lang--on-multi-indent-line nil) ; bool - true if last-line has multiple indent possibiliies
(setq 488-lang--cur-multi-indent nil) ; value of the current displayed option for multi indentation
(setq 488-lang--last-line-number nil) ; line number of the last line processed
(make-local-variable '488-lang--min-line-indent)
(make-local-variable '488-lang--max-line-indent)
(make-local-variable '488-lang--on-multi-indent-line) ; bool - true if last-line has multiple indent options
(make-local-variable '488-lang--cur-multi-indent) ; value of the current displayed option for multi indentation
(make-local-variable '488-lang--last-line-number)



(defun 488-lang-multi-indent ()
  "Cycles multipule indentation levels if we are on a multi-indent line. Return t if indented, nil o/w."
  ;; Check if we are cycling tab options on a multi-indent line
  (if 488-lang--on-multi-indent-line
      ;; Check if on same line as last time
      (if (= 488-lang--last-line-number (what-line-val))
          ;; Give next option, return
          (progn
            (setq 488-lang--cur-multi-indent (+ 488-lang--cur-multi-indent default-tab-width))
            ;; Check for wrap around
            (if (> 488-lang--cur-multi-indent 488-lang--max-line-indent)                
                (setq 488-lang--cur-multi-indent 488-lang--min-line-indent))
            ;; Indent line
            (indent-line-to 488-lang--cur-multi-indent)
            ;; Return success
            t)       
        ;; No longer on same multi-indent line
        (progn
          (setq 488-lang--on-multi-indent-line nil)))    
    ;; Return
    nil))



(defun get-containing-scope-indentation ()
  "Return the indentation level of the containing scope marker ('{') for the current line."
  (let ((scope-indentation nil))
    (save-excursion
      (let ((not-top-scope-found t) (open-brackets 0))
        (while not-top-scope-found
          (forward-line -1)
          (cond
           ((looking-at "^[ \t]*}") ; Newly opened scope
            (setq open-brackets (+ open-brackets 1)))           
           ((looking-at "^[ \t]*{")
            (if (> open-brackets 0)
                (setq open-brackets (- open-brackets 1))
              ;; No open brackets, use scope
              (progn
                (setq scope-indentation (current-indentation))
                (setq not-top-scope-found nil))))           
           ((bobp)
            (progn
              (setq scope-indentation 0)
              (setq not-top-scope-found nil)))))))
    scope-indentation))



(defun get-last-indentation ()
  "Return the indenttion of the last line that contains text."
  (let ((last-indentation nil))
    (save-excursion
      (let ((not-text-found t))
        (while not-text-found
          (forward-line -1)
          (cond
           ;; Find a line that is not only whitespace
           ((looking-at "^[ \t]*[^ \t\n]")
            (progn
              (setq last-indentation (current-indentation))
              (setq not-text-found nil)))           
           ((bobp)
            (progn
              (setq last-indentation 0)
              (setq not-text-found nil)))))))
    last-indentation))



(defun setup-multi-indent ()
  "Set min and max indent range, put indentation in range, and set multi option indentation flag."
  ;; NOTE(sdsmith): max - prev indentation; min - indent level of inner most scope (ie. '{...}')
  (progn
    ;; Find min indentation level (one more than containing scope)
    (setq 488-lang--min-line-indent (+ (get-containing-scope-indentation) default-tab-width))    
    ;; Find max indentation level (previous indentation level)
    (setq 488-lang--max-line-indent (get-last-indentation))
    (if (< 488-lang--max-line-indent 488-lang--min-line-indent)
        ;; Accounts for case when statment directly follows a '{'
        (setq 488-lang--max-line-indent 488-lang--min-line-indent))
    ;; Bring indentation in bounds
    (cond
     ((> (current-indentation) 488-lang--max-line-indent) ; Check upper bound
      (setq 488-lang--cur-multi-indent 488-lang--max-line-indent))
     ((< (current-indentation) 488-lang--min-line-indent) ; Check lower bound
      (setq 488-lang--cur-multi-indent 488-lang--min-line-indent))
     (t ; In bounds, keep it the same
      (setq 488-lang--cur-multi-indent (current-indentation))))

    (setq cur-indent 488-lang--cur-multi-indent) ; Apply indentation
    
    (if (= 488-lang--min-line-indent 488-lang--max-line-indent) ; Only multi indent if bounds are diff
        (setq 488-lang--on-multi-indent-line t)))) ; Set multi option indent flag


(defun 488-lang-indent-line ()
  ;; TODO(sdsmith): There is a lot of room for optimization. For instance, there is no need to `always` check
  ;; for the beginning of the scope.
  "Indent current line as 488 Lang code"

  (progn    
    ;; Check if we are cycling tab options on a multi-indent line
    (if (not (488-lang-multi-indent))      
        ;; Find indentation level
        (progn
          (beginning-of-line) ; Set point to beginning of line
          (if (bobp)
              (indent-line-to 0)
            (let ((not-indented t) cur-indent)
              (progn
                (cond

                 ;; Indent '{' to previous indentation level
                 ((looking-at "^[ \t]*{")
                  (setq cur-indent (get-last-indentation)))

                 ;; Indent scope brackets
                 ((looking-at "^[ \t]*}")
                  ;; Find the top of the scope, and indent it at that level
                  (setq cur-indent (get-containing-scope-indentation)))
                 
                 ;; Indent 'else'
                 ((looking-at "^[ \t]*ELSE")
                  ;; TODO(sdsmith): only do this if we find an if
                  (setq cur-indent (- (get-last-indentation) default-tab-width)))
                 
                 ;; Handle two line control statement keywords
                 ((looking-at "^[ \t]*\\(DO\\|UNTIL\\|THEN\\)")
                  (setq cur-indent (get-last-indentation)))
                 
                 ;; Check for possible multiple options of indentation levels
                 ((looking-at "^[ \t]*\\(IF\\|WHILE\\|REPEAT\\)")
                  (setup-multi-indent))
                 
                 ;; Iterate backward through code to find indentation 'hint'
                 (t
                  (save-excursion
                    (while not-indented
                      (forward-line -1)
                      (cond
                       
                       ;; Line up with closing bracket
                       ((looking-at "^[ \t]*}") ; Rule 3
                        (progn
                          (setq cur-indent (current-indentation))
                          (setq not-indented nil)))
                       
                       ;; Control statement, indent
                       ((looking-at "^[ \t]*\\(THEN\\|ELSE\\|UNTIL\\|DO\\|{\\)")
                        (progn
                          (setq cur-indent (+ (current-indentation) default-tab-width))
                          (setq not-indented nil)))
                       
                       ((bobp) ; No hints if beginning of buffer
                        (progn
                          (setq cur-indent 0)
                          (setq not-indented nil))))))))
                
                ;; Clean-up
                (progn
                  (if cur-indent
                      (progn
                        ;; Sanity check - make sure indent >= 0
                        (if (< cur-indent 0) 
                            (setq cur-indent 0))
                        (indent-line-to cur-indent))
                    (indent-line-to 0))))))

          ;; Record last line
          (setq 488-lang--last-line-number (what-line-val))))))




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



;;; ENTRY FUNCTION
;; Function to be called by emacs when mode is started
(defun 488-lang-mode ()
  "Major mode for CSC488 Source Language (2016 Winter)"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table 488-lang-mode-syntax-table) ; Set syntax table
  (use-local-map 488-lang-mode-map) ; Set key map
  (set (make-local-variable 'font-lock-defaults) '(488-lang-font-lock-keywords)) ; Set font lock
  (set (make-local-variable 'indent-line-function) '488-lang-indent-line) ; Set indent function
  (setq major-mode '488-lang-mode) ; Set major mode
  (setq mode-name "488 Lang") ; Set name to appear in buffer
  (run-hooks '488-lang-mode-hook))
    


;;; EXPOSE MODULE
;; Add mode to 'features' list (ie. exposing it to the emacs environment)
(provide '488-lang-mode)
