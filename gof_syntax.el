(defvar pugofer-mode-syntax-table
  (let ((st (make-syntax-table)))

    ;; Comment: '--' to end of line
    (modify-syntax-entry ?- ". 12" st)
    (modify-syntax-entry ?\n ">" st)

    ;; Strings: "string"
    (modify-syntax-entry ?\" "\"" st)

    ;; Parens and brackets
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)

    ;; Treat underscore and apostrophe as part of words
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?' "w" st)

    ;; Operators and punctuation (e.g. dot, colon, equals)
    (modify-syntax-entry ?. "." st)
    (modify-syntax-entry ?: "." st)
    (modify-syntax-entry ?= "." st)
    (modify-syntax-entry ?| "." st)
    (modify-syntax-entry ?> "." st)
    (modify-syntax-entry ?< "." st)
    (modify-syntax-entry ?+ "." st)
    (modify-syntax-entry ?- "." st)
    (modify-syntax-entry ?* "." st)
    (modify-syntax-entry ?/ "." st)
    (modify-syntax-entry ?\\ "." st)

    st))

(defun pugofer-mode-syntax-fixes ()
  (modify-syntax-entry ?_ "w" pugofer-mode-syntax-table)         ;; Treat _ as part of words
  (modify-syntax-entry ?- ". 12" pugofer-mode-syntax-table)      ;; Start of comment
  (modify-syntax-entry ?\n ">" pugofer-mode-syntax-table)        ;; End of comment
  (modify-syntax-entry ?\" "\"" pugofer-mode-syntax-table))      ;; Strings
