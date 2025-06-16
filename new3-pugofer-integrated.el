;;; pugofer.el --- Major mode for Pugofer language -*- lexical-binding: t; -*-

;; https://claude.ai/chat/a58a3f72-06b6-498f-8565-fe2bfb6425a3
;; 15 June 2025
;; Copyright (C) Original Authors
;; Author: Original Maintainer
;; Maintainer: Rusi P. Mody <rustompmody@gmail.com>
;; Version: 2.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: languages, pugofer
;; URL: https://github.com/pugofer/emacs-pug

;;; Commentary:
;; Major mode for the Pug programming language.
;; Derived from Mark Jones' gofer
;; Part of the Pugofer educational system

;;; Code:

(require 'comint)
(require 'easymenu)

(defgroup pugofer nil
  "Support for the Pugofer language."
  :group 'languages
  :link '(url-link :tag "Repository" "https://github.com/pugofer/emacs-pug"))

;;; Configuration
(defcustom pugofer-program-name "pug"
  "Path to the Pugofer executable."
  :type 'string
  :group 'pugofer)

(defcustom pugofer-prelude-directory nil
  "Directory containing Pugofer prelude files."
  :type '(choice (const nil) directory)
  :group 'pugofer)

(defcustom pugofer-mode-hook nil
  "Hook run when entering `pugofer-mode'."
  :type 'hook
  :group 'pugofer)

(defcustom inferior-pugofer-mode-hook nil
  "Hook run when entering `inferior-pugofer-mode'."
  :type 'hook
  :group 'pugofer)

(defcustom pugofer-compile-exp-command "(compile '%s)"
  "Template for compiling Pugofer expressions."
  :type 'string
  :group 'pugofer)

(defvar pugofer-buffer nil
  "Current Pugofer process buffer.")

;;; Syntax and Font Lock
(defconst pugofer-keywords
  '("%%" "%token" "%type" "case" "class" "data" "else" "if" "in" 
    "infix" "infixl" "infixr" "instance" "let" "of" "then" "type" "where"))

;; (defconst pugofer-font-lock-keywords
;;   `((,(regexp-opt pugofer-keywords 'symbols) . font-lock-keyword-face)
;;     ("\\<\\([A-Z][a-zA-Z0-9_']*\\)\\>" . font-lock-type-face)
;;     ("\\('\\sw+'\\|\\sw+'\\)" . font-lock-variable-name-face)
;;     ("\\(->\\|::\\|=\\|\\\\\\|←\\)" . font-lock-builtin-face)
;;     ("--.*$" . font-lock-comment-face)
;;     ("{-[^-]*\\(?:-[^}][^-]*\\)*-}" . font-lock-comment-face)))

(defconst pugofer-font-lock-keywords
  `((,(regexp-opt pugofer-keywords 'symbols) . font-lock-keyword-face)
    ("\\<\\([A-Z][a-zA-Z0-9_']*\\)\\>" . font-lock-type-face)
    ("\\('\\sw+'\\|\\sw+'\\)" . font-lock-variable-name-face)
    ("\\(->\\|::\\|=\\|\\\\\\|←\\)" . font-lock-builtin-face)
    ("--.*$" . font-lock-comment-face)
    ("{-[[:ascii:]]*?-}" . font-lock-comment-face)))  ; More precise multi-line comment pattern

;; (defvar pugofer-mode-syntax-table
;;   (let ((table (make-syntax-table)))
;;     (modify-syntax-entry ?- ". 12" table)  ; -- comments
;;     (modify-syntax-entry ?\n ">" table)
;;     (modify-syntax-entry ?{ "(}1nb" table) ; {- -} comments
;;     (modify-syntax-entry ?} "){4nb" table)
;;     (modify-syntax-entry ?' "\"" table)    ; ' is string quote for chars
;;     table))

(defvar pugofer-mode-syntax-table
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
(defvar pugofer-source-modes '(pugofer-mode)
  "Modes considered to contain Pugofer source code.")

(defvar pugofer-prev-l/c-dir/file nil
  "Caches the last (directory . file) pair for load/compile commands.")

;;; Keymaps and Menu
(defvar pugofer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "DEL") 'backward-delete-char-untabify)
    (define-key map (kbd "M-C-q") 'pugofer-indent-sexp)
    (define-key map (kbd "M-C-x") 'pugofer-send-definition)
    (define-key map (kbd "C-x C-e") 'pugofer-send-last-sexp)
    (define-key map (kbd "C-c C-e") 'pugofer-send-definition)
    (define-key map (kbd "C-c M-e") 'pugofer-send-definition-and-go)
    (define-key map (kbd "C-c C-r") 'pugofer-send-region)
    (define-key map (kbd "C-c M-r") 'pugofer-send-region-and-go)
    (define-key map (kbd "C-c C-z") 'switch-to-pugofer)
    (define-key map (kbd "C-c C-l") 'pugofer-load-file)
    (define-key map (kbd "C-c C-k") 'pugofer-compile-file)
    map))

(easy-menu-define pugofer-menu pugofer-mode-map "Pugofer Menu"
  '("Pugofer"
    ["Run Constructor" run-pugofer-cc]
    ["Run Standard" run-pugofer-std] 
    ["Run Simple" run-pugofer-simple]
    ["Run with PUP Prelude" run-pug-pug]
    ["Run with CAT Prelude" run-pugofer-cat]
    "---"
    ["Load File" pugofer-load-file]
    ["Compile File" pugofer-compile-file]))

;;; Major Mode
(define-derived-mode pugofer-mode prog-mode "Pugofer"
  "Major mode for editing Pugofer code."
  :group 'pugofer
  :syntax-table pugofer-mode-syntax-table
  
  (setq-local font-lock-defaults '(pugofer-font-lock-keywords))
  (setq-local comment-start "--")
  (setq-local comment-end "")
  (setq-local comment-start-skip "--+\\s-*")
  (setq-local indent-line-function 'lisp-indent-line)
  
  (when (fboundp 'electric-indent-local-mode)
    (electric-indent-local-mode -1))
  
  (easy-menu-add pugofer-menu))

;; Add file extensions to auto-mode-alist
(add-to-list 'auto-mode-alist '("\\.gs\\'" . pugofer-mode))   ;; gofer source
(add-to-list 'auto-mode-alist '("\\.lgs\\'" . pugofer-mode))  ;; literate gofer source
(add-to-list 'auto-mode-alist '("\\.gp\\'" . pugofer-mode))    ;; gofer project files


;; Add interpreter mode for pug REPL
(add-to-list 'interpreter-mode-alist '("pug" . inferior-pugofer-mode))

;;; Process Management
(defvar inferior-pugofer-filter-regexp "\\`\\s *\\S ?\\S ?\\s *\\'"
  "Input matching this regexp is not saved in history.
Ignores very short inputs (0, 1, or 2 non-whitespace characters).")

(defun pugofer-input-filter (str)
  "Return non-nil if STR should be saved in command history."
  (not (string-match-p inferior-pugofer-filter-regexp str)))

(defun pugofer-proc ()
  "Return the active Pugofer process buffer."
  (let ((proc (get-buffer-process (if (derived-mode-p 'inferior-pugofer-mode)
                                      (current-buffer)
                                    pugofer-buffer))))
    (or proc 
        (progn (run-pugofer pugofer-program-name) 
               (pugofer-proc)))))

(defun pu-comint-check-source (fname)
  "Check if buffer for FNAME is modified and save it if needed."
  (let ((buff (get-file-buffer fname)))
    (when (and buff (buffer-modified-p buff))
      (with-current-buffer buff (save-buffer)))))

;;; Inferior Mode
(defvar inferior-pugofer-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    (define-key map (kbd "M-C-x") 'pugofer-send-definition)
    (define-key map (kbd "C-x C-e") 'pugofer-send-last-sexp)
    (define-key map (kbd "C-c C-l") 'pugofer-load-file)
    (define-key map (kbd "C-c C-k") 'pugofer-compile-file)
    map))

(define-derived-mode inferior-pugofer-mode comint-mode "Inferior Pugofer"
  "Major mode for interacting with Pugofer REPL."
  :group 'pugofer
  
  (setq comint-prompt-regexp "^? ")
  (setq comint-input-filter #'pugofer-input-filter)
  (setq mode-line-process '(":%s"))
  (use-local-map inferior-pugofer-mode-map)
  
  (run-hooks 'inferior-pugofer-mode-hook))

;;; Interactive Commands
;;;###autoload
(defun run-pugofer (&optional cmd)
  "Run inferior Pugofer process."
  (interactive
   (list (when current-prefix-arg
           (read-string "Run Pugofer: " pugofer-program-name))))
  
  (let ((cmd (or cmd pugofer-program-name)))
    (unless (comint-check-proc "*pugofer*")
      (let ((buffer (make-comint "pugofer" cmd nil)))
        (with-current-buffer buffer
          (inferior-pugofer-mode))))
    
    (setq pugofer-program-name cmd)
    (setq pugofer-buffer "*pugofer*")
    (pop-to-buffer pugofer-buffer)))

(defun pugofer-send-region (start end)
  "Send region to inferior Pugofer process."
  (interactive "r")
  (comint-send-region (pugofer-proc) start end)
  (comint-send-string (pugofer-proc) "\n"))

(defun pugofer-send-definition ()
  "Send current definition to inferior Pugofer."
  (interactive)
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (pugofer-send-region (point) end))))

(defun pugofer-send-last-sexp ()
  "Send the previous sexp to the inferior Pugofer process."
  (interactive)
  (pugofer-send-region (save-excursion (backward-sexp) (point)) (point)))

(defun pugofer-send-definition-and-go ()
  "Send current definition to Pugofer and switch to process buffer."
  (interactive)
  (pugofer-send-definition)
  (switch-to-pugofer t))

(defun pugofer-send-region-and-go (start end)
  "Send region to Pugofer and switch to process buffer."
  (interactive "r")
  (pugofer-send-region start end)
  (switch-to-pugofer t))

(defun pugofer-load-file (file-name)
  "Load a Pugofer file into the inferior process."
  (interactive "fLoad Pugofer file: ")
  (setq file-name (expand-file-name file-name))
  (pu-comint-check-source file-name)
  (let* ((d (file-name-directory file-name))
         (f (file-name-nondirectory file-name))
         (ftobesent (if (string= "" (file-relative-name d default-directory))
                        f file-name)))
    (setq pugofer-prev-l/c-dir/file (cons d f))
    (comint-send-string (pugofer-proc) (format ":l %s\n" ftobesent))
    (switch-to-pugofer t)))

(defun pugofer-compile-file (file-name)
  "Compile a Pugofer file in the inferior process."
  (interactive "fCompile Pugofer file: ")
  (setq file-name (expand-file-name file-name))
  (pu-comint-check-source file-name)
  (setq pugofer-prev-l/c-dir/file (cons (file-name-directory file-name)
                                        (file-name-nondirectory file-name)))
  (comint-send-string (pugofer-proc) (format "(compile-file \"%s\")\n" file-name)))

(defun switch-to-pugofer (eob-p)
  "Switch to the Pugofer process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (if (get-buffer pugofer-buffer)
      (pop-to-buffer pugofer-buffer)
    (error "No current process buffer. See variable pugofer-buffer"))
  (when eob-p
    (push-mark)
    (goto-char (point-max))))

;;; Indentation
(defun pugofer-indent-sexp ()
  "Indent the current S-expression according to Pugofer rules."
  (interactive)
  (save-excursion
    (let ((end (progn (forward-sexp) (point))))
      (backward-sexp)
      (indent-region (point) end))))

;;; Run commands with different preludes
(defun run-pugofer-cc ()
  "Run Pugofer with constructor classes."
  (interactive)
  (run-pugofer (concat pugofer-program-name " +c")))

(defun run-pugofer-std ()
  "Run standard Pugofer."
  (interactive)
  (run-pugofer pugofer-program-name))


(defun run-pug-pug ()
  "Run Pugofer with PUP prelude."
  (interactive)
  (setenv "PUG" (expand-file-name "pug.pre" pugofer-prelude-directory))
  (run-pugofer pugofer-program-name))


(provide 'pugofer)
;;; pugofer.el ends here
