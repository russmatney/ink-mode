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
(require 'thingatpt)
(require 'outline)
(require 'subr-x)
(require 'easymenu)

(defgroup ink nil
  "Major mode for writing interactive fiction in Ink."
  :group 'languages)

(defvar ink-mode-hook nil)

(defvar ink-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-c") 'ink-play)
    (define-key map (kbd "C-c C-p") 'ink-play-knot)
    (define-key map (kbd "C-c C-o") 'ink-follow-link-at-point)
    ;; Visibility cycling
    (define-key map (kbd "TAB") 'ink-cycle)
    (define-key map (kbd "<S-iso-lefttab>") 'ink-shifttab)
    (define-key map (kbd "<S-tab>")  'ink-shifttab)
    (define-key map (kbd "<backtab>") 'ink-shifttab)
    map)
  "Keymap for ink major mode.")

(defvar ink-mode-mouse-map
  (let ((map (make-sparse-keymap)))
    (define-key map [follow-link] 'mouse-face)
    (define-key map [mouse-2] #'ink-follow-link-at-point)
    map)
  "Keymap for following links with mouse.")

(easy-menu-define ink-mode-menu ink-mode-map
  "Menu for `ink-mode'."
  '("Ink"
    ["Run game from start" ink-play]
    ["Run game from knot or stitch" ink-play-knot]
    "---"
    ["Follow link at point" ink-follow-link-at-point]))

(defconst ink-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; // starts a comment
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    ;; End of line ends a comment
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?\" ".   " st)
    (modify-syntax-entry ?\( ".   " st)
    (modify-syntax-entry ?\) ".   " st)
    (modify-syntax-entry ?\[ ".   " st)
    (modify-syntax-entry ?\] ".   " st)
    (modify-syntax-entry ?0 ".   " st)
    (modify-syntax-entry ?1 ".   " st)
    (modify-syntax-entry ?2 ".   " st)
    (modify-syntax-entry ?3 ".   " st)
    (modify-syntax-entry ?4 ".   " st)
    (modify-syntax-entry ?5 ".   " st)
    (modify-syntax-entry ?6 ".   " st)
    (modify-syntax-entry ?7 ".   " st)
    (modify-syntax-entry ?8 ".   " st)
    (modify-syntax-entry ?9 ".   " st)
    st)
  "Syntax table used while in `ink-mode'.")


;;; Regular Expressions

(defconst ink-regex-header
  "^\\s-*\\(?1:=+\\)\\s-*\\(?2:\\(?:function\\)?\\)\\s-*\\(?3:[[:word:]_.]+\\)\\s-*\\(?4:\\(?:([^)]*)\\)?\\)\\s-*\\(?5:=*\\)"
  "Regexp identifying Ink headers.
Group 1 matches the equal signs preceding the title.
Group 2 matches the function keyword.
Group 3 matches the header title.
Group 4 matches the function arguments.
Group 5 matches the optional equal signs following the header.")

(defconst ink-regex-label
  "^\\(?:\\s-*[*+\\-]\\)+\\s-*\\(?1:(\\(?2:[[:word:]_]+\\))\\)"
  "Regexp identifying Ink diverts.
Group 1 matches a label including parentheses.
Group 2 matches a label excluding parentheses.")

(defconst ink-regex-divert
  "\\(?1:->\\|<-\\)\\s-*\\(?2:[[:word:]_.]*\\)?"
  "Regexp identifying Ink diverts.
Group 1 matches an left or right arrow.
Group 2 matches a link text")

(defconst ink-regex-include
  "^\\s-*\\(?1:INCLUDE\\)\\s-*\\(?2:.*?\\)\\s-*$"
  "Regexp identifying Ink diverts.
Group 1 matches an INCLUDE keyword
Group 2 matches a link text")

(defconst ink-regex-comment
  "^\\s-*\\(TODO\\|//\\|.*?/\\*\\|.*?\\*/\\)"
  "Regexp identifying Ink comments.")


;;; Link following

(defun ink-follow-link-at-point ()
  "Open the current link.
Determine whether it leads to a header or to an included file by
matching regexps."
  (interactive "@")
  (let ((found-link nil))
    (cond ((thing-at-point-looking-at ink-regex-divert)
           (ink-follow-header-or-label-link)
           (setq found-link t))
          ((thing-at-point-looking-at ink-regex-include)
           (ink-follow-file-link)
           (setq found-link t))
          ((not found-link)
           (user-error "No links")))))

(defun ink-find-header (title)
  "Find a header (knot or stitch) matching TITLE in the buffer.
Return its position."
  (let (position)
    (save-excursion
      (goto-char (point-min))
      (while (and (not position)
                  (re-search-forward ink-regex-header (buffer-end 1) t))
        (when (string-equal (ink-get-knot-name) title)
          (setq position (point))))
      position)))

(defun ink-find-label (title-list &optional start end)
  "Find a label matching TITLE-LIST in the buffer.
Return its position.
TITLE-LIST consists of one two three elements, giving four possiblities:
\(label stitch knot\)
\(label stitch\)
\(label knot\)
\(label\)
START and END can specify the range in which
to search."
  ;; reverse title list to get label first
  (setq title-list (reverse title-list))
  (let (position
        (start (if start start (point-min)))
        (end   (if end end (point-max))))
    (save-excursion
      (goto-char start)
      (while (and (not position)
                  (re-search-forward ink-regex-label end t))
        ;; do different checks depending on title list length
        (cond ((and (not position)
                    (= 3 (length title-list))
                    ;; three elements: compare all three
                    (equal (ink-get-label-name) title-list))
               (setq position (point)))
              ((and (not position)
                    (= 2 (length title-list))
                    ;; two elements: compare first and (second or third) elements
                    (and
                     (equal (nth 0 (ink-get-label-name)) (nth 0 title-list))
                     (or
                      (equal (nth 1 (ink-get-label-name)) (nth 1 title-list))
                      (equal (nth 2 (ink-get-label-name)) (nth 1 title-list)))))
               (setq position (point)))
              ((and (not position)
                    (= 1 (length title-list))
                    ;; one element: compare only label
                    (equal (nth 0 (ink-get-label-name)) (nth 0 title-list)))
               (setq position (point))))))
    position))

(defun ink-follow-header-or-label-link ()
  "Go to the header or label matching the link at point."
  (let (position
        title title-list
        knot-name
        stitch-start stitch-end
        knot-start knot-end)
    (font-lock-ensure)
    (setq title (string-trim-right (match-string-no-properties 2) "\\."))
    (if (string-match-p "^\\(\\END\\|DONE\\)" title)
        (user-error "%s is not a real link" title)
      (progn
        (save-excursion
          ;; get knot and stitch names and start / end positions
          (when (ignore-errors (outline-back-to-heading t))
            (if (= (ink-outline-level) 2)
                ;; In stitch
                (progn
                  (setq stitch-start (point))
                  (save-excursion
                    (ink-end-of-subtree t)
                    (setq stitch-end (point)))
                  (ignore-errors (outline-up-heading 1))
                  (setq knot-name (ink-get-knot-name))
                  (setq knot-start (point))
                  (save-excursion
                    (ink-end-of-subtree t)
                    (setq knot-end (point))))
              ;; In knot
              (setq knot-name (ink-get-knot-name))
              (setq knot-start (point))
              (save-excursion
                (ink-end-of-subtree t)
                (setq knot-end (point))))))

    ;; Look for header
    (setq position (ink-find-header title))
    ;; Look for stitch with that name in current knot:
    (if (not position)
        (setq position (ink-find-header (concat knot-name "." title))))

    ;; Look for labels:
    (setq title-list (split-string title "\\."))
    (unless position
      (cond ((or (= 1 (length title-list))
                 (= 2 (length title-list)))
             ;; Title has one or two element;
             ;; look in order in stitch, knot and outside
             (if (and (not position)
                      stitch-start)
                 (setq position
                       (ink-find-label title-list stitch-start stitch-end)))
             (if (and (not position)
                      knot-start)
                 (setq position
                       (ink-find-label title-list knot-start knot-end)))
             (if (not position)
                 (setq position
                       (ink-find-label title-list))))

            ;; Title has three elements;
            ;; look as knot.stitch.label in whole buffer
            ((and (not position)
                  (= 3 (length title-list)))
             (setq position (ink-find-label title-list)))))

    (if position (progn
                   (message "Jumping to %s" title)
                   (goto-char position)
                   (ignore-errors (outline-show-subtree)))
      (user-error "Link `%s' not found. Is it in another file?" title))))))

(defun ink-follow-file-link ()
  "Find file matching the link at point."
  (let (file-name)
    (setq file-name (match-string-no-properties 2))
    (setq file-name (concat (file-name-directory
                             (buffer-file-name))
                            file-name))
    (find-file file-name)
    (message "Visiting %s" file-name)))


;;; Faces

(defface ink-shadow-face
  '((t (:inherit shadow)))
  "Face for Ink headers and glue."
  :group 'ink-faces)

(defface ink-knot-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for Ink knots: == * ==."
  :group 'ink-faces)

(defface ink-stitch-face
  '((t (:inherit 'ink-knot-face)))
  "Face for Ink stitches: = *."
  :group 'ink-faces)

(defface ink-link-face
  '((t (:inherit link)))
  "Face for Ink divert links."
  :group 'ink-faces)

(defface ink-arrow-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for Ink divert arrows."
  :group 'ink-faces)

(defface ink-tag-face
  '((t (:inherit font-lock-doc-face)))
  "Face for Ink tags: ()."
  :group 'ink-faces)

(defface ink-bracket-face
  '((t (:inherit italic)))
  "Face for Ink brackets: []."
  :group 'ink-faces)


;;; Highlighting

(defun ink-fontify-diverts (last)
  "Add text properties to next divert from point to LAST."
  (when (re-search-forward ink-regex-divert last t)
    (ink-fontify-links
     ;; Arrow part
     (list 'face 'ink-arrow-face
           'rear-nonsticky t
           'font-lock-multiline t))
    t))

(defun ink-fontify-includes (last)
  "Add text properties to next include from point to LAST."
  (when (re-search-forward ink-regex-include last t)
    (ink-fontify-links
     ;; INCLUDE part
     (list 'face 'font-lock-keyword-face
           'rear-nonsticky t
           'font-lock-multiline t))
    t))

(defun ink-fontify-links (pre-part)
  "Add text properties to link.
Use the PRE-PART list as properties to fontify the part preceding
the link, whether it be an arrow for diverts, or the INCLUDE
keyword."
  (let* ((link-start (match-beginning 2))
         (link-end (match-end 2))
         (title (string-trim-right (match-string-no-properties 2) "\\."))
         ;; Link part (without face)
         (lp (list 'keymap ink-mode-mouse-map
                   'mouse-face 'highlight
                   'font-lock-multiline t
                   'help-echo (if title title ""))))
    (when (match-end 1)
      (add-text-properties (match-beginning 1) (match-end 1) pre-part))
    (when link-start
      (add-text-properties link-start link-end lp)
      (add-face-text-property link-start link-end
                              'ink-link-face 'append))
    t))

(defvar ink-font-lock-keywords
  `(
    ;; TODO-style comments
    ("^\\s-*\\(TODO.*\\)" . font-lock-comment-face)

    ;; Knots
    (,ink-regex-header
     (1 'ink-shadow-face)
     (2 font-lock-keyword-face)
     (3 'ink-knot-face)
     (4 font-lock-variable-name-face)
     (5 'ink-shadow-face))

    ;; Diverts, threads and tunnels
    (ink-fontify-diverts)

    ;; Labels
    (,ink-regex-label 1 font-lock-variable-name-face)

    ;; Choices
    ("^\\s-*\\([*+]\\s-*\\)+" . font-lock-type-face)

    ;; Ties
    ("^\\s-*\\(\\(?:\\s-*-\\)+\\)\\(?:[^>]\\|$\\)" 1 font-lock-type-face)

    ;; Keywords at beginning of line
    ("^\\s-*\\(VAR\\|CONST\\|LIST\\)" . font-lock-keyword-face)

    ;; Includes
    (ink-fontify-includes)

    ;; Vars, constants and lists
    ("^\\s-*\\(?:VAR\\|CONST\\|LIST\\)\\s-+\\([[:word:]_]+\\)" 1
     font-lock-variable-name-face)

    ;; Conditions
    ("{.*?\\(:\\).*?}" 1 font-lock-constant-face)

    ;; Alternatives
    ("\\(?:^\\|[^\\\\]\\)\\([{|}]+\\)" 1 font-lock-constant-face)

    ;; Code lines
    ("\\(^\\s-*~\\)" (0 font-lock-type-face)
     ("\\_<\\(?:return\\|temp\\|ref\\)\\_>" nil nil (0 font-lock-keyword-face))
     ("\\(\".*?\"\\)" nil nil (0 font-lock-string-face))
     ("\\([[:word:]_]+\\)(.*)" nil nil (1 font-lock-function-name-face))
     ("\\_<\\(?1:.*?\\)\\s-*\\(?2:=\\)\\_>" nil nil
      (1 font-lock-variable-name-face))
     ("\\_<\\(SEED_RANDOM\\|RANDOM\\|CHOICE_COUNT\\|TURNS\\|TURNS_SINCE\\|INT\\|FLOOR\\|FLOAT\\)\\_>" nil nil (0 font-lock-builtin-face)))

    ;; Tags
    ("\\(?:^\\|[^\\\\]\\)\\(#.*\\)$" 1 'ink-tag-face)

    ;; Glue
    ("\\(^\\s-*<>\\|<>\\s-*$\\)" . 'ink-shadow-face)

    ;; Brackets
    ("^\\(?:\\s-*[*+]\\).*\\(\\[.*\\]\\)" 1 'ink-bracket-face)))


;;; Indentation

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
  ;; The follow-indentation-p trick was taken from python-mode.el
  (let ((follow-indentation-p
         ;; Check if point is within indentation.
         (and (<= (line-beginning-position) (point))
              (>= (+ (line-beginning-position)
                     (current-indentation))
                  (point)))))
    (save-excursion
      (indent-line-to (ink-calculate-indentation)))
    (ink-indent-choices)
    (when follow-indentation-p (back-to-indentation))))

(defun ink-indent-choices ()
  "Indent choices and ties: add indentations between symbols."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (and (looking-at "^\\s-*[*+\\-]")
               (not (looking-at ".*\\*/")))
      (while
          (re-search-forward
           "\\(?:[*+\\-]>?\\s-*\\)*?\\(?:[*+\\-]\\(?1:>\\)?\\)\\(?2:\\s-*\\)"
           (line-end-position) t)
        (replace-match
         (if (match-beginning 1) " "
           (if indent-tabs-mode
               "\t"
             (make-string (max 0 (- tab-width 1)) ? )))
         nil nil nil 2)))))

(defun ink-calculate-bracket-indentation ()
  "Find the level of bracket and condition blocks.
Opening brackets indent, as do conditions.
Closing brackets dedent."
  (let (
        (bracket-difference 0)
        (indentation-list (list))
        (start-pos) (on-last-line))
    (save-excursion
      ;; Go back to header or buffer start
      (setq start-pos (point))
      (or
       (ignore-errors (outline-back-to-heading t))
       (goto-char (point-min)))

      (while (not on-last-line)
        ;; Exit condition: on starting line
        (when (= (line-number-at-pos)
                 (line-number-at-pos start-pos))
          (setq on-last-line t))

        ;; Count the difference between opening and closing brackets
        (when (or
               (looking-at "^\\s-*{.*")
               (looking-at ".*}.*$"))
          (setq bracket-difference
                (-
                 (count-matches
                  "\\({\\)"
                  (line-beginning-position)
                  (line-end-position))
                 (count-matches
                  "\\(}\\)"
                  (line-beginning-position)
                  (line-end-position)))))

        ;; Increase indent level on opening bracket
        (when
            (and (looking-at "^\\s-*{.*")
                 (> bracket-difference 0))
          (unless on-last-line
            (push 'b indentation-list)))

        ;; Increase indent on condition
        (when (looking-at "^\\s-*-.*:\\s-*$")
          (let (previous-indent)
            (setq previous-indent (nth 0 indentation-list))
            (if on-last-line
                (when (eq previous-indent 'c)
                  (pop indentation-list))
              (unless (eq previous-indent 'c)
                (push 'c indentation-list)))))

        ;; Decrease indent level on closing bracket
        (when
            (and (looking-at ".*}.*$") (< bracket-difference 0))

          (let (previous-indent)
            (setq previous-indent (nth 0 indentation-list))
            ;; Dedent if previous indent was a bracket in a list
            (when (eq previous-indent 'c)
              ;; Check whether next line also opens a bracket, in which case
              ;; don't dedent yet
              (save-excursion
                (goto-char start-pos)
                (forward-line 1)
                (unless (looking-at "^\\s-*{.*")
                  (pop indentation-list))))
            (pop indentation-list)))

        (unless (bobp)
          (forward-line 1))))
    indentation-list))

(defun ink-calculate-indentation ()
  "Find indent level at point."
  (beginning-of-line)
  (let ((indented nil) (cur-indent 0)
        (bracket-level 0))

    ;; Knot or stitch: indent at 0
    (when (looking-at ink-regex-header)
      (setq indented t))

    (when (not indented)
      (setq bracket-level (length (ink-calculate-bracket-indentation))))

    (cond
     ;; Empty lines
     ((looking-at "^\\s-*$")
      (setq cur-indent 0)
      (setq indented t))

     ;; Choice * +
     ((and (looking-at "^\\s-*[*+]")
           (not (looking-at ".*?\\*/")))
      (setq cur-indent (ink-count-choices))
      (setq indented t))

     ;; Tie -
     ((and (looking-at "^\\s-*\\(-[^>]\\|-$\\)")
           (not (looking-at ".*?\\*/")))
      (setq cur-indent (- (ink-count-choices) 1))
      (setq indented t))

     ;; Brackets
     ((looking-at "^\\s-*{")
      (setq cur-indent 0)
      (setq indented t))
     ((looking-at ".*}\\s-*$")
      (setq cur-indent 0)
      (setq indented t))

     ((not indented)
      ;; If not choice, tie, knot, stitch or first line
      (save-excursion
        (if (looking-at ink-regex-comment)
            ;; Comment // or TODO: look down until we find
            ;; something which isn't a comment, then find
            ;; _that_ indent
            (let ((found-not-comment nil))
              (while (not found-not-comment)
                (forward-line 1)
                (if (or
                     (= (line-number-at-pos)
                        (line-number-at-pos (point-max)))
                     (looking-at ink-regex-comment))
                    (setq found-not-comment t)
                  ;; Found something that’s not a comment
                  (progn
                    (setq cur-indent (ink-calculate-indentation))
                    (setq indented t)
                    (setq found-not-comment t)))))

          ;; Not a comment
          (while (not indented)
            ;; Go up until we find something
            (forward-line -1)
            (cond
             ;; Choice * +
             ((looking-at "^\\s-*[*+]")
              (setq cur-indent (* (ink-count-choices) 2))
              (setq indented t))
             ;; Tie -
             ((and (looking-at "^\\s-*\\(-[^>]\\|-$\\)")
                   (not (looking-at ".*?\\*/"))
                   (not (looking-at ".*:\\s-*$")))
              (setq cur-indent (- (* (ink-count-choices) 2) 1))
              (setq indented t))
             ;; Condition - ... :
             ((looking-at "^\\s-*-.*:")
              (setq cur-indent 0)
              (setq indented t))
             ;; Closing brackets: skip them
             ((and (looking-at ".*}\\s-*$")
                   (not (looking-at "^\\s-*{")))
              (beginning-of-line)
              ;; Check whether line matching bracket was a condition
              (search-forward "}")
              (backward-list 1))
             ((or (bobp) (looking-at "^\\s-*="))
              ;; First line of buffer, knot or stitch
              (setq indented t))))))))
    (if cur-indent
        (progn
          (max 0
               (* (+ cur-indent bracket-level)
                  tab-width)))
      0)))


;;; Ink-play

(defcustom ink-inklecate-path (executable-find "inklecate")
  "The path to the Inklecate executable."
  :group 'ink
  :type '(file))

(defvar ink-comint-do-filter nil)

(defun ink-play-knot ()
  "Play the current ink buffer from the knot or stitch at point."
  (interactive)
  (ink-play t))

(defun ink-play (&optional go-to-knot)
  "Play the current ink buffer.
If the GO-TO-KNOT optional argument is non-nil, start at the knot
or stitch at point. In that case we issue \"-> knot.stitch\" to
the process, and suppress the beginning output using the comint
output filter."
  (interactive "P")
  (let* ((file-name (buffer-file-name))
         (ink-buffer
          (if (comint-check-proc "*Ink*")
              (progn
                (comint-exec "*Ink*" "Ink" ink-inklecate-path
                             nil `("-p" ,file-name))
                "*Ink*")
            (make-comint "Ink" ink-inklecate-path nil
                         "-p" (buffer-file-name))))
         (knot-name (ink-get-knot-name)))
    (switch-to-buffer-other-window ink-buffer)
    (comint-clear-buffer)
    (if (and go-to-knot knot-name)
        (progn
          (setq ink-comint-do-filter t)
          (message (concat "Running " knot-name "..."))
          (comint-send-string (get-process "Ink")
                              (concat "-> " knot-name "\n"))
          (comint-delete-output)
          (comint-clear-buffer))
      (setq ink-comint-do-filter nil))
    (message "Running Ink...")))

(defun ink-filter-output-line (line)
  "Filter single line of text from Inklecate's output.
The filter is active only on starting play. It outputs all
errors, warnings and infos appearing in LINE, and discards the
rest."
  (let ((result ""))
    (if ink-comint-do-filter
        (cond ((string-match-p "^\\(ERROR:\\|WARNING:\\|TODO\\)" line)
               (setq result (concat line "\n")))
              ((string-match-p "\\?>" line)
               (setq result (concat (substring line 3) "\n"))
               (setq ink-comint-do-filter nil))
              ((not result)
               (setq result "")))
      (setq result (concat line "\n")))
    result))

(defun ink-comint-filter-output (output)
  "Comint output filter for ink-play.
This whole filter is just so that the first output of comint
doesn't print before the first important line when starting
directly at a knot... OUTPUT is the output to be filtered."
  (if ink-comint-do-filter
      (setq output (mapconcat #'ink-filter-output-line (split-string output "\n") "")))
  output)

(add-hook 'comint-preoutput-filter-functions #'ink-comint-filter-output)


;;; Error checking with flycheck

(when (boundp 'flycheck-checkers)
  (flycheck-def-executable-var ink-flycheck-checker "inklecate")
  (flycheck-define-command-checker 'ink-flycheck-checker
    "An ink syntax checker using the Inklecate compiler.

See URL `https://www.inklestudios.com/ink/'."
    ;; :command (ink-inklecate-path source)
    :command `(,ink-inklecate-path source-inplace)
    :error-patterns
    '((warning ;line-start
       "WARNING: '" (file-name)
       "' line " line ": "
       (message) line-end)
      (error ;line-start
       "ERROR: '" (file-name)
       "' line " line ": "
       (message) line-end)
      (info ;line-start
       "TODO: '" (file-name)
       "' line " line ": "
       (message) line-end))
    :modes 'ink-mode))


;;; Outline

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

(defun ink-get-knot-name ()
  "Return the name of the knot at point, or knot.stitch if in stitch."
  (save-excursion
    (let ((knot-name ""))
      (when (ignore-errors (outline-back-to-heading t))
        (re-search-forward ink-regex-header)
        (setq knot-name (match-string-no-properties 3))
        (if (= (ink-outline-level) 2)
            ;; Currently in stitch, go up to look at knot
            (progn
              (ignore-errors (outline-up-heading 1))
              (re-search-forward ink-regex-header)
              (setq knot-name
                    (concat (match-string-no-properties 3) "."
                            knot-name))))
        knot-name))))

(defun ink-get-label-name ()
  "Return the name of the label at point.
Can also be knot.label if in knot, or knot.stitch.label if in
stitch."
  (save-excursion
    (beginning-of-line)
    (let (
          knot-name
          (title-list (list)))
      (re-search-forward ink-regex-label (line-end-position) t)
      (setq title-list (list (match-string-no-properties 2)))
      (setq knot-name (ink-get-knot-name))
      (if (and knot-name title-list)
          (setq title-list (append title-list (reverse (split-string knot-name "\\.")))))
      title-list)))

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
   ((thing-at-point-looking-at ink-regex-header)
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


;;; Mode Definition

;;;###autoload
(define-derived-mode ink-mode prog-mode "Ink"
  "Major mode for editing interactive fiction using the Ink
  scripting language."
  :syntax-table ink-mode-syntax-table

  ;; Syntax
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "//+\\s-*")
  (setq-local comment-use-syntax t)
  (setq-local comment-end "")
  (setq-local comment-auto-fill-only-comments t)
  (setq font-lock-defaults '(ink-font-lock-keywords))

  ;; Indent
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local indent-line-function #'ink-indent-line)

  ;; Outline
  (setq-local outline-regexp ink-regex-header)
  (setq-local outline-level #'ink-outline-level)
  ;; Cause use of ellipses for invisible text.
  (add-to-invisibility-spec '(outline . t))

  ;; Flycheck
  (when (fboundp 'flycheck-mode-on-safe)
    (add-to-list 'flycheck-checkers 'ink-flycheck-checker t)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ink\\'" . ink-mode))

(provide 'ink-mode)
;;; ink-mode.el ends here
