;;; ink-mode.el --- Major mode for writing interactive fiction in Ink -*- lexical-binding: t -*-

;; Copyright (C) 2016-2020 Erik Sjöstrand, Damien Picard, and
;; ink-mode contributors (see the commit log for details).

;; Author: Erik Sjöstrand, Damien Picard
;; URL: http://github.com/Kungsgeten/ink-mode
;; Version: 0.2
;; Package-Version: 20200522
;; Keywords: languages, wp, hypermedia
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; `ink-mode' provides basic syntax highlighting and indentation for
;; the Ink scripting language, developed by Inkle Studios. There's
;; also a command `ink-play' to playtest your story from Emacs (bound
;; to C-c C-c by default).

;;; Code:
(require 'rx)
(require 'comint)
(require 'outline)

(defgroup ink nil
  "Major mode for writing interactive fiction in Ink."
  :group 'languages)

(defvar ink-mode-hook nil)

(defvar ink-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-c") 'ink-play)
    ;; Visibility cycling
    (define-key map (kbd "TAB") 'ink-cycle)
    (define-key map (kbd "<S-iso-lefttab>") 'ink-shifttab)
    (define-key map (kbd "<S-tab>")  'ink-shifttab)
    (define-key map (kbd "<backtab>") 'ink-shifttab)
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

(defconst ink-regex-header
  "^\\s-*\\(?:\\(?1:=+\\)[ \t]+\\(?2:.*?\\)\\(?3:[ \t]*=*\\)[ \t]*\\)$"
  "Regexp identifying Ink headings.
Group 1 matches a single opening equal sign of a heading.
Group 2 matches the text, without surrounding whitespace, of a heading.
Group 3 matches the closing whitespace and equal signs of a heading.")

(defconst ink-regex-comment
  "^\\s-*\\(TODO\\|//\\|.*?/\\*\\|.*?\\*/\\)"
  "Regexp identifying Ink comments.")


;;; Faces =======================================================

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


;;; Highlighting =======================================================

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


;;; Indentation =======================================================

(defun ink-count-choices ()
  "Return number of choices or ties in line."
  (interactive)
  (let ((choices 0))
    (save-excursion
      (beginning-of-line)
      (re-search-forward
       "\\(?1:\\(?:[*+-]\\s-*\\)+?\\)\\s-*\\(?:->\\)?\\s-*\\([^*+-]\\|$\\)"
       (line-end-position) t)
      (if (match-beginning 0)
          (setq choices (count-matches "\\([*+-]\\)" (line-beginning-position) (match-end 1)))))
    choices))

(defun ink-indent-line ()
  "Indent current line of Ink code."
  (save-excursion
    (indent-line-to (ink-calculate-indentation))))

(defun ink-indent-choices ()
  "Indent choices and ties: add indentations between symbols."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (while (re-search-forward "\\([*+-]\\)\\(\\s-*\\)" (line-end-position) t)
      (if (looking-at ">")
          (goto-char (line-end-position))
        (replace-match
         (if indent-tabs-mode
             "\t"
           (make-string (max 0 (- tab-width 1)) ? ))
         nil nil nil 2))
      (if (looking-at "^[:space:]-")
          (goto-char (line-end-position))))))

(defun ink-calculate-indentation ()
  "Find indent level at point."
  (beginning-of-line)
  (let ((not-indented t) cur-indent)
    (cond ((or (bobp) (looking-at "^\\s-*="))
           ;; First line of buffer or knot or stitch
           (setq not-indented nil))
          ((and (looking-at "^\\s-*[*+]")
                (not (looking-at ".*?\\*/")))
           ;; Choice * +
           (setq cur-indent (* (ink-count-choices) tab-width))
           (setq not-indented nil)
           (ink-indent-choices))
          ((and (looking-at "^\\s-*\\(-[^>]\\|-$\\)")
                (not (looking-at ".*?\\*/")))
           ;; Tie -
           (setq cur-indent (* (- (ink-count-choices) 1) tab-width))
           (setq not-indented nil)
           (ink-indent-choices))
          (not-indented
           ;; if not choice, tie, knot, stitch or first line
           (save-excursion
             (if (looking-at ink-regex-comment)
                 ;; Comment // or TODO: look down until we find
                 ;; something which isn't a comment, then find
                 ;; _that_ indent
                 (let ((not-comment-not-found t))
                   (while not-comment-not-found
                     (forward-line 1)
                     (unless (looking-at ink-regex-comment)
                       ;; Found something that’s not a comment
                       (progn
                         (setq cur-indent (ink-calculate-indentation))
                         (setq not-indented nil)
                         (setq not-comment-not-found nil)))))
               (while not-indented
                 ;; Go up until we find something
                 (forward-line -1)
                 (cond
                  ((looking-at "^\\s-*[*+]")
                   ;; Choice * +
                   (setq cur-indent (* (ink-count-choices) 2 tab-width))
                   (setq not-indented nil))
                  ((and (looking-at "^\\s-*\\(-[^>]\\|-$\\)")
                        (not (looking-at ".*?\\*/")))
                   ;; Tie -
                   (setq cur-indent (* (- (* (ink-count-choices) 2) 1) tab-width))
                   (setq not-indented nil))
                  ((or (bobp) (looking-at "^\\s-*="))
                   ;; First line of buffer, knot or stitch
                   (setq not-indented nil))))))))
    (if cur-indent
        cur-indent
      0)))


;;; Ink-play =======================================================

(defcustom ink-inklecate-path (executable-find "inklecate")
  "The path to the Inklecate executable."
  :group 'ink
  :type '(file))

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


;;; Outline  ==========================================================

;; Outline functions were derived from markdown-mode.el, in turn
;; originally derived from from org.el.

(defvar ink-cycle-global-status 1)
(defvar ink-cycle-subtree-status nil)

(defalias 'ink-end-of-heading 'outline-end-of-heading)

(defun ink-end-of-subtree (&optional invisible-OK)
  "Move to the end of the current subtree.
Only visible heading lines are considered, unless INVISIBLE-OK is
non-nil.
Derived from `markdown-end-of-subtree', derived from `org-end-of-subtree'."
  (outline-back-to-heading invisible-OK)
  (let ((first t)
        (level (ink-outline-level)))
    (while (and (not (eobp))
                (or first (> (ink-outline-level) level)))
      (setq first nil)
      (outline-next-heading))
    (if (memq (preceding-char) '(?\n ?\^M))
        (progn
          ;; Go to end of line before heading
          (forward-char -1)
          (if (memq (preceding-char) '(?\n ?\^M))
              ;; leave blank line before heading
              (forward-char -1)))))
  (point))

(defun ink-on-heading-p ()
  "Return non-nil if point is on a heading line."
  (save-excursion
    (beginning-of-line)
    (re-search-forward
     ink-regex-header
     (line-end-position) t)))

(defun ink-shifttab ()
  "S-TAB keybinding: cycle global heading visibility by calling `ink-cycle' with argument t."
  (interactive)
  (ink-cycle t))

(defun ink-cycle (&optional arg)
  "Visibility cycling for Ink mode.
If ARG is t, perform global visibility cycling. If the point is
at a header, cycle visibility of the corresponding subtree.
Otherwise, indent the current line or insert a tab, as
appropriate, by calling `indent-for-tab-command'."
  (interactive "P")
  (cond

   ;; Global cycling
   ((eq arg t)
    (cond
     ;; Move from overview to contents
     ((and (eq last-command this-command)
           (eq ink-cycle-global-status 2))
      (outline-hide-sublevels 1)
      (message "CONTENTS")
      (setq ink-cycle-global-status 3))
     ;; Move from contents to all
     ((and (eq last-command this-command)
           (eq ink-cycle-global-status 3))
      (outline-show-all)
      (message "SHOW ALL")
      (setq ink-cycle-global-status 1))
     ;; Defaults to overview
     (t
      (outline-hide-body)
      (message "OVERVIEW")
      (setq ink-cycle-global-status 2))))

   ;; At a heading: rotate between three different views
   ((save-excursion (beginning-of-line 1) (ink-on-heading-p))
    (outline-back-to-heading)
    (let (eoh eol eos)
      ;; Determine boundaries
      (save-excursion
        (outline-back-to-heading)
        (save-excursion
          (beginning-of-line 2)
          (while (and (not (eobp)) ;; this is like `next-line'
                      (get-char-property (1- (point)) 'invisible))
            (beginning-of-line 2)) (setq eol (point)))
        (ink-end-of-heading)   (setq eoh (point))
        (ink-end-of-subtree t)
        (skip-chars-forward " \t\n")
        (beginning-of-line 1) ; in case this is an item
        (setq eos (1- (point))))
      ;; Find out what to do next and set `this-command'
      (cond
       ;; Nothing is hidden behind this heading
       ((= eos eoh)
        (message "EMPTY ENTRY")
        (setq ink-cycle-subtree-status nil))
       ;; Entire subtree is hidden in one line: open it
       ((>= eol eos)
        ;; (ink-show-entry)
        (outline-show-entry)
        (outline-show-children)
        (message "CHILDREN")
        (setq ink-cycle-subtree-status 'children))
       ;; We just showed the children, now show everything.
       ((and (eq last-command this-command)
             (eq ink-cycle-subtree-status 'children))
        (outline-show-subtree)
        (message "SUBTREE")
        (setq ink-cycle-subtree-status 'subtree))
       ;; Default action: hide the subtree.
       (t
        (outline-hide-subtree)
        (message "FOLDED")
        (setq ink-cycle-subtree-status 'folded)))))

   ;; Otherwise, indent as appropriate
   (t
    (indent-for-tab-command))))

(defun ink-outline-level ()
  "Return the depth to which a statement is nested in the outline."
  (if (> (length (match-string-no-properties 1)) 1)
      1
    2))


;;; Mode Definition  ==========================================================

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
  (setq-local indent-line-function #'ink-indent-line)
  ;; Outline
  (setq-local outline-regexp ink-regex-header)
  (setq-local outline-level #'ink-outline-level)
  ;; Cause use of ellipses for invisible text.
  (add-to-invisibility-spec '(outline . t))
  (setq font-lock-defaults '(ink-font-lock-keywords)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ink\\'" . ink-mode))

(provide 'ink-mode)
;;; ink-mode.el ends here
