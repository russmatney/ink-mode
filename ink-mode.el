;;; ink-mode.el --- Major mode for writing interactive fiction in Ink -*- lexical-binding: t -*-

;; Copyright (C) 2016 Erik Sjöstrand
;; MIT License

;; Author: Erik Sjöstrand
;; URL: http://github.com/Kungsgeten/ink-mode
;; Version: 0.1
;; Keywords: languages
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;; `ink-mode' provides basic syntax highlightning for the Ink scripting
;; language, developed by Inkle Studios.  There's also a command `ink-play' to
;; playtest your story from Emacs (bound to C-c C-c by default).

;;; Code:
(require 'rx)
(require 'comint)

(defgroup ink nil
  "Major mode for writing interactive fiction in Ink."
  :group 'languages)

(defvar ink-mode-hook nil)

(defvar ink-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-c") 'ink-play)
    map)
  "Keymap for ink major mode.")

(defconst ink-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; // starts a comment
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    ;; End of line ends a comment
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?\" "w" st)
    st))

(defface ink-shadow-face
  '((t (:inherit shadow)))
  "Face for Ink headers and glue."
  :group 'ink-faces)

(defface ink-knot-face
  '((t (:inherit font-lock-string-face)))
  "Face for Ink knots: == * ==."
  :group 'ink-faces)

(defface ink-stitch-face
  '((t (:inherit 'ink-knot-face)))
  "Face for Ink stitches: = *."
  :group 'ink-faces)

(defface ink-tag-face
  '((t (:inherit font-lock-doc-face)))
  "Face for Ink tags: ()."
  :group 'ink-faces)

(defface ink-bracket-face
  '((t (:inherit italic)))
  "Face for Ink brackets: []."
  :group 'ink-faces)

(defvar ink-font-lock-keywords
  `(
    ;; TODO-style comments
    ("^\\s-*\\(TODO.*\\)" . font-lock-comment-face)

    ;; Knots
    ;; ^\s*(={2,})\s*(function)?\s*(\w+)\s*(\([^)]*\))?\s*(={1,})?
    ("^\\s-*\\(=\\{2,\\}\\)\\s-*\\(\\(?:function\\)?\\)\\s-*\\([[:word:]_]+\\)\\s-*\\(\\(?:([^)]*)\\)?\\)\\s-*\\(\\(?:=\\{1,\\}\\)?\\)"
     (1 'ink-shadow-face)
     (2 font-lock-keyword-face)
     (3 'ink-knot-face)
     (4 font-lock-variable-name-face)
     (5 'ink-shadow-face))

    ;; Stitches
    ;; ^\s*(=)\s*(\w+)\s*(\([^)\n]*\))?\s*$
    ("^\\s-*\\(=\\)\\s-*\\([[:word:]_]+\\)\\s-*\\(\\(:?([^)\n]*)\\)?\\)$"
     (1 'ink-shadow-face)
     (2 'ink-stitch-face)
     (3 font-lock-variable-name-face))

    ;; Diverts, threads and tunnels
    ("\\(\\(?:->\\|<-\\)+\\)\\s-*\\([[:word:]_]*\\)"
     (1 font-lock-builtin-face)
     (2 'ink-knot-face))

    ;; Labels
    (,(rx bol (0+ whitespace)
          (1+ (or whitespace "*" "+" "-"))
          (group "(" (1+ not-newline) ")"))
     1 font-lock-variable-name-face)

    ;; Choices
    ("^\\s-*\\([*+]\\s-*\\)+" . font-lock-type-face)

    ;; Ties
    ("^\\s-*\\(\\(?:\\s-*-\\)+\\)\\(?:[^>]\\|$\\)" 1 font-lock-type-face)

    ;; Keywords at beginning of line
    ("^\\s-*\\(VAR\\|CONST\\|INCLUDE\\|LIST\\)" . font-lock-keyword-face)

    ;; Vars, constants and lists
    ("^\\s-*\\(?:VAR\\|CONST\\|LIST\\)\\s-+\\([[:word:]_]+\\)" 1
     font-lock-variable-name-face)

    ;; Conditions
    ("\\({.*?:\\).*?}" 1 font-lock-constant-face)
    ("^[[:space:]*+]+\\({.*?}\\)" 1 font-lock-constant-face)

    ;; Alternatives
    ("\\(?:^\\|[^\\\\]\\)\\([{|}]+\\)" 1 font-lock-constant-face)

    ;; Code lines
    (,(rx bol (0+ whitespace)
          (group "~") (1+ not-newline)) . font-lock-type-face)

    ;; Tags
    ("\\(?:^\\|[^\\\\]\\)\\(#.*\\)$" 1 'ink-tag-face)

    ;; Glue
    ("\\(^\\s-*<>\\|<>\\s-*$\\)" . 'ink-shadow-face)

    ;; Brackets
    ("^\\(?:\\s-*[*+]\\).*\\(\\[.*\\]\\)" 1 'ink-bracket-face)))

(defvar ink-inklecate-path (executable-find "inklecate")
  "The path to the Inklecate executable.")

(defun ink-play ()
  "Play the current ink buffer."
  (interactive)
  (let ((buffer (comint-check-proc "Ink")))
    (pop-to-buffer-same-window
     (if (or buffer (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer "*Ink*"))
       (current-buffer)))
    (unless buffer
      (switch-to-buffer-other-window
       (apply 'make-comint-in-buffer "Ink" buffer
              ink-inklecate-path nil `("-p" ,(buffer-file-name)))))))

;;;###autoload
(define-derived-mode ink-mode
  text-mode "Ink"
  "Major mode for editing interactive fiction using the Ink
  scripting language."
  :syntax-table ink-mode-syntax-table
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "//+\\s-*")
  (setq-local comment-use-syntax t)
  (setq-local comment-end "")
  (setq-local comment-auto-fill-only-comments t)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq font-lock-defaults '(ink-font-lock-keywords)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ink\\'" . ink-mode))

(provide 'ink-mode)
;;; ink-mode.el ends here
