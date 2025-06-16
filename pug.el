;;; pug.el --- Major mode for Pug language -*- lexical-binding: t; -*-

;; https://claude.ai/chat/a58a3f72-06b6-498f-8565-fe2bfb6425a3
;; https://chat.deepseek.com/a/chat/s/b93780e0-bf5e-408d-a716-4fea9e72e1e9
;; 15 June 2025
;; Copyright (C) Original Authors
;; Author: Original Maintainer
;; Maintainer: Rusi P. Mody <rustompmody@gmail.com>
;; Version: 2.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: languages, pug
;; URL: https://github.com/pugofer/emacs-pug

;;; Commentary:
;; Major mode for the Pug programming language.
;; Derived from Mark Jones' gofer
;; Part of the Pugofer educational system

;;; Code:

(require 'comint)
(require 'easymenu)

(defgroup pug nil
  "Support for the Pug language."
  :group 'languages
  :link '(url-link :tag "Repository" "https://github.com/pugofer/emacs-pug"))

;;; Configuration
(defcustom pug-program-name "pug"
  "Path to the Pug executable."
  :type 'string
  :group 'pug)

(defcustom pug-langlevel-directory  nil
  "Directory containing Pug langlevel files."
  :type '(choice (const nil) directory)
  :group 'pug)

(defcustom pug-mode-hook nil
  "Hook run when entering `pug-mode'."
  :type 'hook
  :group 'pug)

(defcustom inferior-pug-mode-hook nil
  "Hook run when entering `inferior-pug-mode'."
  :type 'hook
  :group 'pug)

(defcustom pug-compile-exp-command "(compile '%s)"
  "Template for compiling Pug expressions."
  :type 'string
  :group 'pug)

(defvar pug-buffer nil
  "Current Pug process buffer.")

;;; Syntax and Font Lock
(defconst pug-keywords
  '("%%" "%token" "%type" "case" "class" "data" "else" "if" "in" 
    "infix" "infixl" "infixr" "instance" "let" "of" "then" "type" "where"
    "primitive" "do"
    "ctype"))

;; (defconst pug-font-lock-keywords
;;   `((,(regexp-opt pug-keywords 'symbols) . font-lock-keyword-face)
;;     ("\\<\\([A-Z][a-zA-Z0-9_']*\\)\\>" . font-lock-type-face)
;;     ("\\('\\sw+'\\|\\sw+'\\)" . font-lock-variable-name-face)
;;     ("\\(->\\|::\\|=\\|\\\\\\|←\\)" . font-lock-builtin-face)
;;     ("--.*$" . font-lock-comment-face)
;;     ("{-[[:ascii:]]*?-}" . font-lock-comment-face)

;;     ;; Numbers (integers, floats, hex, octal)
;;     ("\\<\\([0-9]+\\|0[xX][0-9a-fA-F]+\\|0[oO][0-7]+\\)\\>" . font-lock-constant-face)
;;     ("\\<[0-9]*\\.[0-9]+\\([eE][+-]?[0-9]+\\)?\\>" . font-lock-constant-face)

;;     ;; Characters and strings
;;     ("'\\(?:\\\\[\\\"'\\\\nrt]\\|[^'\\\\]\\)'" . font-lock-string-face)
;;     ("\"\\(?:\\\\[\\\"'\\\\nrt]\\|[^\\\\\"]\\)*\"" . font-lock-string-face)

;;     ;; Arithmetic and comparison operators
;;     ("\\(\\+\\+\\|\\*\\*\\|==\\|/=\\|<=\\|>=\\|&&\\|||\\|>>\\|<<\\)" . font-lock-builtin-face)
;;     ("\\([+*/<>-]\\|`[a-zA-Z_][a-zA-Z0-9_']*`\\)" . font-lock-builtin-face)

;;     ;; Special symbols
;;     ("\\(|\\|@\\|~\\|\\.\\.\\.\\)" . font-lock-builtin-face)

    
;;     ))  ; More precise multi-line comment pattern

(defconst pug-font-lock-keywords
  `((,(regexp-opt pug-keywords 'symbols) . font-lock-keyword-face)
    ("\\<\\([A-Z][a-zA-Z0-9_']*\\)\\>" . font-lock-type-face)
    ("\\('\\sw+'\\|\\sw+'\\)" . font-lock-variable-name-face)
    ("\\(->\\|::\\|=\\|\\\\\\|←\\)" . font-lock-builtin-face)
    ("--.*$" . font-lock-comment-face)
    ("{-[[:ascii:]]*?-}" . font-lock-comment-face)

    ;; Numbers (integers, floats, hex, octal)
    ("\\<\\([0-9]+\\|0[xX][0-9a-fA-F]+\\|0[oO][0-7]+\\)\\>" . font-lock-constant-face)
    ("\\<[0-9]*\\.[0-9]+\\([eE][+-]?[0-9]+\\)?\\>" . font-lock-constant-face)

    ;; Characters and strings
    ("'\\(?:\\\\[\\\"'\\\\nrt]\\|[^'\\\\]\\)'" . font-lock-string-face)
    ("\"\\(?:\\\\[\\\"'\\\\nrt]\\|[^\\\\\"]\\)*\"" . font-lock-string-face)

    ;; Fixed arithmetic and comparison operators
    ;; Multi-character operators with word boundaries
    ("\\<\\(\\+\\+\\|\\*\\*\\|==\\|/=\\|<=\\|>=\\|&&\\|||\\|>>\\|<<\\)\\>" . font-lock-builtin-face)
    
    ;; Single-character operators with whitespace checks
    ("\\(?:\\s-\\|^\\)\\([+*/<>-]\\)\\(?:\\s-\\|$\\)" 1 font-lock-builtin-face)
    
    ;; Backtick operators
    ("`[a-zA-Z_][a-zA-Z0-9_']*`" . font-lock-builtin-face)

    ;; Special symbols
    ("\\(|\\|@\\|~\\|\\.\\.\\.\\)" . font-lock-builtin-face)
    ))

(defvar pug-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Single-line comments
    (modify-syntax-entry ?- ". 12" table)  ; -- starts comment
    (modify-syntax-entry ?\n ">" table)    ; newline ends comment
    
    ;; Multi-line comments {- ... -}
    (modify-syntax-entry ?{  "(}1n" table) ; { starts comment, n means nestable
    (modify-syntax-entry ?-  ". 23" table) ; - is part of comment delimiters
    (modify-syntax-entry ?}  ")(4n" table) ; } ends comment
    
    ;; Character literals
    (modify-syntax-entry ?' "\"" table)    ; ' is string quote for chars
    table))

;;; Variables for tracking state
(defvar pug-source-modes '(pug-mode)
  "Modes considered to contain Pug source code.")

(defvar pug-prev-l/c-dir/file nil
  "Caches the last (directory . file) pair for load/compile commands.")

;;; Keymaps and Menu
(defvar pug-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "DEL") 'backward-delete-char-untabify)
    (define-key map (kbd "M-C-q") 'pug-indent-sexp)
    (define-key map (kbd "M-C-x") 'pug-send-definition)
    (define-key map (kbd "C-x C-e") 'pug-send-last-sexp)
    (define-key map (kbd "C-c C-e") 'pug-send-definition)
    (define-key map (kbd "C-c M-e") 'pug-send-definition-and-go)
    (define-key map (kbd "C-c C-r") 'pug-send-region)
    (define-key map (kbd "C-c M-r") 'pug-send-region-and-go)
    (define-key map (kbd "C-c C-z") 'switch-to-pug)
    (define-key map (kbd "C-c C-l") 'pug-load-file)
    (define-key map (kbd "C-c C-k") 'pug-compile-file)
    map))

(easy-menu-define pug-menu pug-mode-map "Pug Menu"
  '("Pug"
    ["Run Constructor" run-pug-cc]
    ["Run Standard" run-pug-std] 
    ["Run Simple" run-pug-simple]
    ["Run with PUG Langlevel" run-pug-pug]
    ["Run with CAT Langlevel" run-pug-cat]
    "---"
    ["Load File" pug-load-file]
    ["Compile File" pug-compile-file]))

;;; Major Mode
(define-derived-mode pug-mode prog-mode "Pug"
  "Major mode for editing Pug code."
  :group 'pug
  :syntax-table pug-mode-syntax-table
  
  (setq-local font-lock-defaults '(pug-font-lock-keywords))
  (setq-local comment-start "--")
  (setq-local comment-end "")
  (setq-local comment-start-skip "--+\\s-*")
  (setq-local indent-line-function 'lisp-indent-line)
  
  (when (fboundp 'electric-indent-local-mode)
    (electric-indent-local-mode -1))
  
  (easy-menu-add pug-menu))

;; Add file extensions to auto-mode-alist
(add-to-list 'auto-mode-alist '("\\.gs\\'" . pug-mode))   ;; gofer source
(add-to-list 'auto-mode-alist '("\\.lgs\\'" . pug-mode))  ;; literate gofer source
(add-to-list 'auto-mode-alist '("\\.gp\\'" . pug-mode))    ;; gofer project files


;; Add interpreter mode for pug REPL
(add-to-list 'interpreter-mode-alist '("pug" . inferior-pug-mode))

;;; Process Management
(defvar inferior-pug-filter-regexp "\\`\\s *\\S ?\\S ?\\s *\\'"
  "Input matching this regexp is not saved in history.
Ignores very short inputs (0, 1, or 2 non-whitespace characters).")

(defun pug-input-filter (str)
  "Return non-nil if STR should be saved in command history."
  (not (string-match-p inferior-pug-filter-regexp str)))

(defun pug-proc ()
  "Return the active Pug process buffer."
  (let ((proc (get-buffer-process (if (derived-mode-p 'inferior-pug-mode)
                                      (current-buffer)
                                    pug-buffer))))
    (or proc 
        (progn (run-pug pug-program-name) 
               (pug-proc)))))

(defun pu-comint-check-source (fname)
  "Check if buffer for FNAME is modified and save it if needed."
  (let ((buff (get-file-buffer fname)))
    (when (and buff (buffer-modified-p buff))
      (with-current-buffer buff (save-buffer)))))

;;; Inferior Mode
(defvar inferior-pug-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    (define-key map (kbd "M-C-x") 'pug-send-definition)
    (define-key map (kbd "C-x C-e") 'pug-send-last-sexp)
    (define-key map (kbd "C-c C-l") 'pug-load-file)
    (define-key map (kbd "C-c C-k") 'pug-compile-file)
    map))

(define-derived-mode inferior-pug-mode comint-mode "Inferior Pug"
  "Major mode for interacting with Pug REPL."
  :group 'pug
  
  (setq comint-prompt-regexp "^? ")
  (setq comint-input-filter #'pug-input-filter)
  (setq mode-line-process '(":%s"))
  (setq-local font-lock-defaults '(pug-font-lock-keywords))
  (use-local-map inferior-pug-mode-map)
  
  (run-hooks 'inferior-pug-mode-hook))

;;; Interactive Commands
;;;###autoload
(defun run-pug (&optional cmd)
  "Run inferior Pug process."
  (interactive
   (list (when current-prefix-arg
           (read-string "Run Pug: " pug-program-name))))
  
  (let ((cmd (or cmd pug-program-name)))
    (unless (comint-check-proc "*pug*")
      (let ((buffer (make-comint "pug" cmd nil)))
        (with-current-buffer buffer
          (inferior-pug-mode))))
    
    (setq pug-program-name cmd)
    (setq pug-buffer "*pug*")
    (pop-to-buffer pug-buffer)))

(defun pug-send-region (start end)
  "Send region to inferior Pug process."
  (interactive "r")
  (comint-send-region (pug-proc) start end)
  (comint-send-string (pug-proc) "\n"))

(defun pug-send-definition ()
  "Send current definition to inferior Pug."
  (interactive)
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (pug-send-region (point) end))))

(defun pug-send-last-sexp ()
  "Send the previous sexp to the inferior Pug process."
  (interactive)
  (pug-send-region (save-excursion (backward-sexp) (point)) (point)))

(defun pug-send-definition-and-go ()
  "Send current definition to Pug and switch to process buffer."
  (interactive)
  (pug-send-definition)
  (switch-to-pug t))

(defun pug-send-region-and-go (start end)
  "Send region to Pug and switch to process buffer."
  (interactive "r")
  (pug-send-region start end)
  (switch-to-pug t))

(defun pug-load-file (file-name)
  "Load a Pug file into the inferior process."
  (interactive "fLoad Pug file: ")
  (setq file-name (expand-file-name file-name))
  (pu-comint-check-source file-name)
  (let* ((d (file-name-directory file-name))
         (f (file-name-nondirectory file-name))
         (ftobesent (if (string= "" (file-relative-name d default-directory))
                        f file-name)))
    (setq pug-prev-l/c-dir/file (cons d f))
    (comint-send-string (pug-proc) (format ":l %s\n" ftobesent))
    (switch-to-pug t)))

(defun pug-compile-file (file-name)
  "Compile a Pug file in the inferior process."
  (interactive "fCompile Pug file: ")
  (setq file-name (expand-file-name file-name))
  (pu-comint-check-source file-name)
  (setq pug-prev-l/c-dir/file (cons (file-name-directory file-name)
                                        (file-name-nondirectory file-name)))
  (comint-send-string (pug-proc) (format "(compile-file \"%s\")\n" file-name)))

(defun switch-to-pug (eob-p)
  "Switch to the Pug process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (if (get-buffer pug-buffer)
      (pop-to-buffer pug-buffer)
    (error "No current process buffer. See variable pug-buffer"))
  (when eob-p
    (push-mark)
    (goto-char (point-max))))

;;; Indentation
(defun pug-indent-sexp ()
  "Indent the current S-expression according to Pug rules."
  (interactive)
  (save-excursion
    (let ((end (progn (forward-sexp) (point))))
      (backward-sexp)
      (indent-region (point) end))))

;;; Run commands with different langlevels
(defun run-pug-cc ()
  "Run Pug with constructor classes."
  (interactive)
  (run-pug (concat pug-program-name " +c")))

(defun run-pug-std ()
  "Run standard Pug."
  (interactive)
  (run-pug pug-program-name))


(defun run-pug-pug ()
  "Run Pug with PUP langlevel."
  (interactive)
  (setenv "PUG" (expand-file-name "pug.pre" pug-langlevel-directory))
  (run-pug pug-program-name))

;; 1. Define your bright REPL banner face
(defface inferior-pug-banner-face
  '((t :foreground "#FF2D00" 
;       :background "#D35400"  ; Orange background for visibility
       :weight bold 
;      :box (:line-width 1 :color "#F39C12")
       :height 1.1
       :underline t))
  "Face for inferior-pug mode REPL banners."
  :group 'pug)

;; 2. REPL-specific banner markers
(defvar inferior-pug-banner-start-regexp "^Pug Version")
(defvar inferior-pug-banner-end-regexp "^\\?\\s-")
					; ? prompt with following space


;; 3. Function to highlight REPL banner
(defun inferior-pug-highlight-banner ()
  "Highlight the REPL banner section."
  (when (derived-mode-p 'inferior-pug-mode)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward inferior-pug-banner-start-regexp nil t)
        (let ((start (match-beginning 0)))
          (when (re-search-forward inferior-pug-banner-end-regexp nil t)
;;	    (goto-char (- (point) 3))
            (add-text-properties
             start (- (match-end 0) 3)
             '(face inferior-pug-banner-face
               font-lock-face inferior-pug-banner-face
               read-only t       ; Make banner non-editable
               front-sticky t
               rear-nonsticky t))))))))

;; 4. Add to inferior-pug-mode-hook
(add-hook 'inferior-pug-mode-hook #'inferior-pug-highlight-banner)

;; 5. Add to comint output filter (for dynamic REPL updates)
(defun inferior-pug-banner-output-filter (output)
  "Check for banners in REPL output."
  (when (string-match inferior-pug-banner-start-regexp output)
    (inferior-pug-highlight-banner)))
  
(add-hook 'comint-output-filter-functions 
          #'inferior-pug-banner-output-filter)




(provide 'pug)
;;; pug.el ends here
