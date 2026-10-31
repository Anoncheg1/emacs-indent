;;; indent-advices.el --- Core replacement of indentation commands for Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 2026 github - Anoncheg1

;; Author: <github.com/Anoncheg1,codeberg.org/Anoncheg>
;; Keywords: org
;; URL: https://codeberg.org/Anoncheg/emacs-indent
;; Version: 0.1
;; Created: 24 oct 2024
;; Package-Requires: ((emacs "29.1"))
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; License

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Licensed under the GNU Affero General Public License, version 3 (AGPLv3)
;; <https://www.gnu.org/licenses/agpl-3.0.en.html>


;;; Commentary:
;;
;; (add-to-list 'load-path "/path/to/folder/emacs-indent")
;; (require 'indent-advices)

;; Redundant:
(require 'abbrev)
(require 'dabbrev)
(require 'minibuffer)


;;; Code:

(defcustom indent-for-tab-steps
  (list
   #'indent-for-tab-step-1-region-to-column
   #'indent-for-tab-step-2-region-fill-prefix
   #'indent-for-tab-step-3-region-indent-lines
   #'indent-for-tab-step-4-insert-tab
   #'indent-for-tab-step-5-indent-line
   #'indent-for-tab-step-6-remove-spaces-at-point
   #'indent-for-tab-step-7-completion)
  "List of steps to perform in the `indent-for-tab-command' function.
Stops at the first function that returns non nil."
  :type '(repeat function)
  :group 'indent)


;; (defun indent-region-column (start end &optional column)
;;   "Indent to COLUMN region defined by START and END."
;;   (interactive "r\nP")
;;   (setq column (prefix-numeric-value column))
;;     (save-excursion
;;       (goto-char end)
;;       (setq end (point-marker))
;;       (goto-char start)
;;       (or (bolp) (forward-line 1))
;;       (while (< (point) end)
;;         (delete-region (point) (progn (skip-chars-forward " \t") (point)))
;;         (or (eolp)
;;             (indent-to column 0))
;;         (forward-line 1))
;;       (move-marker end nil)))

;; ;; Replace indent-region-column entirely with:
;; (indent-rigidly start end (- column (current-column)))

(defun indent-region-fill-prefix (start end)
  "Insert `fill-prefix' at the beginning of each line in region.
Argument START and END define region."
  (interactive "r")
  (when fill-prefix
    (let ((regexp (regexp-quote fill-prefix))
          (end (copy-marker end)))
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (unless (or (looking-at regexp) (looking-at-p "[ \t]*$"))
            (insert fill-prefix))
          (forward-line 1)))
      (set-marker end nil))))


(defun indent-for-tab-step-1-region-to-column (&optional arg)
  "Indent the region if active.
If a numeric prefix is given, indent to that column.
Optional argument ARG required for this case."
  (when (and arg (use-region-p))
    ;; (indent-region-column (region-beginning) (region-end) arg)
    (call-interactively #'indent-rigidly) ; return value ignored start end (- column (current-column)))
    t))

(defun indent-for-tab-step-2-region-fill-prefix (&optional arg)
  "Indent the region if `fill-prefix' variable is set.
Apply `indent-region-fill-prefix', that put prefix before every line.
Optional argument ARG not used here."
  (ignore arg)
  (when (and fill-prefix (use-region-p))
    (save-restriction
      (widen)
      (indent-region-fill-prefix (region-beginning) (region-end)) ; return values ignored
      (deactivate-mark t)
      t)))

(defun indent-for-tab-step-3-region-indent-lines (&optional arg)
  "Indent the region if active.
If a numeric prefix is given, indent to that column.
Return value
Optional argument ARG not used here."
  (ignore arg)
  (when (use-region-p)
    (if indent-region-function
        (save-restriction
          (widen)
          (funcall indent-region-function (region-beginning) (region-end))) ; return value ignored.
      ;; Else, use a default implementation that calls indent-line-function on
      ;; each line.
      (save-restriction
        (widen)
        (indent-region-line-by-line (region-beginning) (region-end))))
    (deactivate-mark t)
    t))

(defun indent-for-tab-step-4-insert-tab (&optional arg)
  "Insert a literal tab character if necessary.
Insert only when `indent-line-function' is `indent-to-left-margin', or when
`tab-always-indent' is nil and point is past the indentation.
Numeric ARG is the repeat count."
  (when (or (eq indent-line-function 'indent-to-left-margin)
             (and (not tab-always-indent)
                  (or (> (current-column) (current-indentation))
                      (eq this-command last-command))))
    (insert-tab arg)
    t))

(defun indent-following-sexp (old-indent)
  "Shift the rest of the current sexp by the current line's indent change.
OLD-INDENT is the indentation before the line was reindented."
  (let ((delta (- (current-indentation) old-indent)))
    (unless (zerop delta)
      (let ((end (save-excursion
                   (beginning-of-line)
                   (condition-case nil
                       (forward-sexp)
                     (error nil)
                     (user-error nil))
                   (point))))
        (save-excursion
          (forward-line 1)
          (when (< (point) end)
            (indent-rigidly (point) end delta)))))))


(defun indent-for-tab-step-5-indent-line (&optional arg)
  "Indent the current line.
Falling back to the default `indent-line-function' if no change
 occurs (unless inside a comment or when completion is preferred).
Halt execution if `indent-line-function' returns `noindent'.
With ARG (universal), also indent the following sexp when indentation
 moved or modified the buffer.
Return non-nil if indentation occurred or was halted by `noindent'."
  (let ((old-tick (buffer-chars-modified-tick))
        (old-indent (current-indentation))
        ;; - first indent attempt
        (halted (eq 'noindent (indent--funcall-widened indent-line-function))))
    (when (and (not halted)
               (eql old-indent (current-indentation)))
      (or (indent--default-inside-comment) ; should return True
          (when (or (<= (current-column) (current-indentation))
                    (not (eq tab-always-indent 'complete)))
            ;; - second indent attempt
            (indent--funcall-widened (default-value 'indent-line-function)))))

    (let ((moved-or-modified (or (not (eql old-indent (current-indentation)))
                                 (not (eql old-tick (buffer-chars-modified-tick))))))
      (when (and arg moved-or-modified)
        (indent-following-sexp old-indent))
      ;; - return
      (or moved-or-modified
          halted)))) ; halted with 'noindent

(defsubst repeated-command-p () ; not used
  "Return non-nil if the current command is a consecutive repetition."
  (eq last-command this-command))


(defun indent-for-tab-step-6-remove-spaces-at-point (&optional arg)
  "Remove spaces if cursor at them, left one.
Return non-nil if space was removed.
ARG ignored."
  (ignore arg)
  (let ((cur-syn (syntax-class (syntax-after (point)))) ; at cur position
        (next-syn (unless (eobp)
                    (syntax-class (syntax-after (1+ (point))))))
        (prev-syn (unless (bobp)
                    (syntax-class (syntax-after (1- (point)))))))

    ;; if space at current position
    (when (and (eq cur-syn 0) ; space current char
               (or
                (eq next-syn 0)
                (eq prev-syn 0)) ; space before or after also
               (not (looking-back "^\\s-*" (line-beginning-position))) ; not before line
               (not (looking-at "\\s-*$"))            ; nothing but spaces/tabs to EOL
               )
      (let ((old-tick (buffer-chars-modified-tick)))
        (call-interactively #'fixup-whitespace)
        (not (eql old-tick (buffer-chars-modified-tick))))))) ; return t if some was removed

(defun indent-for-tab-step-7-completion (&optional arg)
  "Perform completion if necessary based on nearby characters.
Info for Characters classes at \"(elisp) Syntax Table Internals\".
ARG ignored."
  (ignore arg)
  (when (and (not (bobp))
             (eq tab-always-indent 'complete)
             (let ((cur-syn (syntax-class (syntax-after (point)))) ; at cur position
                   (prev-syn (syntax-class (syntax-after (1- (point))))) ; before cur pos: may be nil, 12 at begining
                   (bmt-old (buffer-modified-tick)))
               (when (and
                      ;; Prev is word or symbol constituent
                      prev-syn (memq prev-syn '(2 3))
                      ;; current character is ;; or end of buffer or not word.
                      (or (null cur-syn) (not (memq cur-syn '(2 3)))))
                 ;; call
                 (or (expand-abbrev) ; return t if was found
                     (completion-at-point) ; return t if was found
                     (condition-case err ; dabbrev-expand without errors
                         (progn (dabbrev-expand arg)
                                (message "dabbrev-expand succeeded")
                                t)
                       (user-error (message "%s" (error-message-string err)) nil))))))))
                       ;; (error (message "dabbrev-expand failed: %s" (error-message-string err)) nil))))))))


(defun indent-for-tab-command--replace (&optional arg)
  "Indent the current line or region, or insert a tab, as appropriate.
This function either inserts a tab, or indents the current line,
or performs symbol completion, depending on `tab-always-indent'.
The function called to actually indent the line or insert a tab
is given by the variable `indent-line-function'.

If a prefix argument is given (ARG), after this function indents the
current line or inserts a tab, it also rigidly indents the entire
balanced expression which starts at the beginning of the current
line, to reflect the current line's indentation.

In most major modes, if point was in the current line's
indentation, it is moved to the first non-whitespace character
after indenting; otherwise it stays at the same position relative
to the text.

If `transient-mark-mode' is turned on and the region is active,
this function instead calls `indent-region-column'.  In this case, any
prefix argument is ignored.

To control exact sequence `indent-for-tab-steps' variable used."
  (interactive "P")
  (seq-find (lambda (step) ; check with `when`
                (message (symbol-name step)) ; debug
                (funcall step arg))
            indent-for-tab-steps))


(defun indent-region-like-first-line (start end)
  "Indent all lines like first between START and END.
Apply `indent-according-to-mode' to the first line.
Intended for usage with `indent-region-function'.
And indent rigidly others."
  (save-excursion
    (goto-char start)
    (beginning-of-line)                                 ; normalize to BOL even if START was mid-line
    (let ((bol (point))
          (old (current-indentation))
          (n   (count-lines (point) end)))
      (indent-according-to-mode nil) ; widen in `ndent-for-tab-step-3-region-indent-lines`
      (when (> n 1)
        (indent-rigidly (line-beginning-position 2)     ; start at BOL of line 2 — line 1 already reindented
                        (save-excursion
                          (goto-char bol)
                          (forward-line (1- n))
                          (line-end-position))
                        (- (current-indentation) old)))))) ; deactivate mark in `ndent-for-tab-step-3-region-indent-lines`

(defun indent-relative-bol-only ()
  "Use `indent-relative' only if cursor at the beginig of the line."
  (when (looking-back "^\\s-*" (line-beginning-position))
    (indent-relative)))

(defun my-indent-region-no-fill-prefix (orig &rest args)
  "Call ORIG with ARGS while `fill-prefix' is bound to nil."
  (let ((fill-prefix nil))
    (apply orig args)))

;;  ---------------------------------------------------------------------------------
(setq indent-line-function #'indent-relative-bol-only)

(setq indent-line-ignored-functions '(indent-relative
                                      indent-relative-maybe
                                      indent-relative-first-indent-point
                                      indent-relative-bol-only))

(advice-add 'indent-for-tab-command :override #'indent-for-tab-command--replace)

;; Dont use fill-repfix for indent-region
(advice-add 'indent-region :around #'my-indent-region-no-fill-prefix)

(provide 'indent-advices)
;;; indent-advices.el ends here
