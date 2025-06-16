;; Setups for emacs for using pugofer mode
;; Assumes you have pugofer.el on the load-path

(add-to-list 'load-path (expand-file-name "~/.emacs.d/setups"))
(autoload 'pugofer-mode "pugofer.el" nil t)
(autoload 'run-pugofer "pugofer.el" nil t)
(autoload 'pugofer-project "pugofer.el" nil t)
(setq pugofer-program-name (expand-file-name "~/gofer/src/gofer"))
(set-variable 'auto-mode-alist
      (append '(
	      ("\\.gs$" . pugofer-mode)   ;; gofer source
	      ("\\.lgs$" . pugofer-mode)   ;; literate gofer source
	      ("\\.gp$" . pugofer-project) ;; gofer project files
	      ) auto-mode-alist))


;; These should go into pugofer.el
;; For now you need to hand-change these paths

(defun run-pug-pup ()
  (interactive)
  (setenv "PUG" (expand-file-name "pup.pre"
				      pugofer-prelude-directory))
  (run-pugofer pugofer-program-name))

(defun run-pug-pug ()
  (interactive)
  (setenv "PUG" (expand-file-name "pug.pre"
				      pugofer-prelude-directory))
  (run-pugofer pugofer-program-name))

(defun run-pug-kit ()
  (interactive)
  (setenv "PUGOFER" (expand-file-name "~/gofer/pucc28.pre"))
  (run-pugofer pugofer-program-name))
