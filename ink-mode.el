(require 'rx)

(defvar ink-mode-hook nil)

(defvar ink-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-j") 'newline-and-indent)
    map)
  "Keymap for ink major mode")

(defconst ink-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; // starts a comment
    (modify-syntax-entry ?/ ". 12" st)
    ;; End of line ends a comment
    (modify-syntax-entry ?\n ">" st)
    st))

(defface ink-condition-face
  '((t :inherit font-lock-type-face))
  "Face for conditions in ink-mode")

(defvar ink-font-lock-keywords
  `(
    ("^=+ *[[:word:]_]+\\(?:(.*)\\)? *=*" .
     font-lock-function-name-face) ;; Knots, functions and stitches
    ("\\(?:->\\|<-\\) *[[:word:]_.]+\\(?:(.*)\\)? *$" .
     font-lock-function-name-face) ;; Diverts and threads
    (,(rx bol (or "VAR" "CONST" "INCLUDE") word-end) .
     font-lock-keyword-face) ;; Keywords
    ("^\\(?:VAR\\|CONST\\) +\\([[:word:]_]+\\)" 1
     font-lock-variable-name-face) ;; Vars/constants
    (,(rx bol (zero-or-more whitespace)
          (one-or-more (or " " "*" "+"))
          (group "{" (one-or-more not-newline) "}"))
     1 font-lock-type-face) ;; Conditions
    (,(rx bol (zero-or-more whitespace)
          "~" (one-or-more not-newline)) . font-lock-type-face)
    ))

(define-derived-mode ink-mode
    prog-mode "Ink"
  "Major mode for editing interactive fiction using the Ink
  scripting language"
  :syntax-table ink-mode-syntax-table
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "//+ *")
  (setq-local comment-use-syntax t)
  (setq-local comment-end "")
  (setq-local comment-auto-fill-only-comments t)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq font-lock-defaults '(ink-font-lock-keywords)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ink\\'" . ink-mode))

(provide 'ink-mode)
