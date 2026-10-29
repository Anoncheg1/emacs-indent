;;; indent-advices.el --- Core replacement of indentation commands for Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 2026 github - Anoncheg1



;;; Commentary:
;;

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


(defun indent-region-column (start end &optional column)
  "Indent to COLUMN region defined by START and END."
  (interactive "r\nP")
  (setq column (prefix-numeric-value column))
    (save-excursion
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (or (bolp) (forward-line 1))
      (while (< (point) end)
        (delete-region (point) (progn (skip-chars-forward " \t") (point)))
        (or (eolp)
            (indent-to column 0))
        (forward-line 1))
      (move-marker end nil)))

(defun indent-region-fill-prefix (start end)
  "Insert `fill-prefix' for region defined by START and END."
  (interactive "r")
  (save-excursion
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (let ((regexp (regexp-quote fill-prefix)))
        (while (< (point) end)
          (or (looking-at regexp)
              (and (bolp) (eolp))
              (insert fill-prefix))
          (forward-line 1)))))


(defun indent-region-fill-prefix (start end)
  "Insert `fill-prefix' for region defined by START and END."
  (interactive "r")
  (save-excursion
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (let ((regexp (regexp-quote fill-prefix)))
        (while (< (point) end)
          (or (looking-at regexp)
              (and (bolp) (eolp))
              (insert fill-prefix))
          (forward-line 1)))))



(defun indent-for-tab-step-1-region-to-column (&optional arg)
  "Indent the region if it is activated.
If a numeric prefix is given, indent to that column.
Optional argument ARG required for this case."
  (when (and arg (use-region-p))
    (indent-region-column (region-beginning) (region-end) arg)
    (setq deactivate-mark t)))

(defun indent-for-tab-step-2-region-fill-prefix (&optional arg)
  "If `fill-prefix' variable is set `indent-region-fill-prefix' used.
Simply prefix before every line.
Optional argument ARG not used here."
  (ignore arg)
  (when (and fill-prefix (use-region-p))
    (indent-region-fill-prefix (region-beginning) (region-end))
    (setq deactivate-mark t)))

(defun indent-for-tab-step-3-region-indent-lines (&optional arg)
  "Indent the region if it is activated.
If a numeric prefix is given, indent to that column.
Optional argument ARG not used here."
  (ignore arg)
  (when (use-region-p)
    (if indent-region-function
        (save-restriction
          (widen)
          (funcall indent-region-function (region-beginning) (region-end)))
      ;; Else, use a default implementation that calls indent-line-function on
      ;; each line.
      (save-restriction
        (widen)
        (indent-region-line-by-line (region-beginning) (region-end))))
    (setq deactivate-mark t)))

(defun indent-for-tab-step-4-insert-tab (&optional arg)
  "Insert a literal tab character if necessary.
Insert only when `indent-line-function' is `indent-to-left-margin', or when
`tab-always-indent' is nil and point is past the indentation.
Numeric ARG is the repeat count."
  (when (or (eq indent-line-function 'indent-to-left-margin)
             (and (not tab-always-indent)
                  (or (> (current-column) (current-indentation))
                      (eq this-command last-command))))
    (insert-tab arg)))

(defun indent-following-sexp (old-indent)
  "Rigidly indent the following sexp if a prefix argument was given.
Used in `indent-for-tab-step-3-indent-line'."
    (let ((end-marker
           (save-excursion
             (forward-line 0) (forward-sexp) (point-marker)))
          (indentation-change (- (current-indentation) old-indent)))
      (save-excursion
        (forward-line 1)
        (when (and (not (zerop indentation-change))
                   (< (point) end-marker))
          (indent-rigidly (point) end-marker indentation-change)))))

(defun indent-for-tab-step-5-indent-line (&optional arg)
  "Indent the current line.
Halt execution if `indent-line-function' returns `noindent'.
If universal argument was given indent following sexp.
Return non-nil if indentation occured or was forcely halted."
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
Return non-nil if space was removed."
  (ignore arg)
  (let ((cur-syn (syntax-class (syntax-after (point)))) ; at cur position
        (next-syn (and (< (point) (point-max))
                       (syntax-class (syntax-after (1+ (point))))))
        (prev-syn (and (> (point) (point-min))
                       (syntax-class (syntax-after (1- (point))))))
        point-save mark-s)

    ;; if space at current position
    (when (and (eq cur-syn 0) ; space current char
               (or
                (eq next-syn 0)
                (eq prev-syn 0)) ; space before or after also
               (not (looking-back "^\\s-*" (line-beginning-position))) ; not before line
               (not (eolp)) ; not at the end of line (spaces removed at saving)
               (not (looking-at "\\s-*$"))            ; NEW: nothing but spaces/tabs to EOL
               )
      (let ((old-tick (buffer-chars-modified-tick)))
        (fixup-whitespace)
        (not (eql old-tick (buffer-chars-modified-tick))))))) ; return t if some was removed

(defun indent-for-tab-step-7-completion (&optional arg)
  "Perform completion if necessary based on nearby characters.
Info for Characters classes at \"(elisp) Syntax Table Internals\"."
  (ignore arg)
  (when (and (eq tab-always-indent 'complete)
             (let ((cur-syn (syntax-class (syntax-after (point)))) ; at cur position
                   (prev-syn (syntax-class (syntax-after (1- (point))))) ; before cur pos: may be nil, 12 at begining
                   (bmt-old (buffer-modified-tick)))
               (when (and
                      ;; Prev is word or symbol constituent
                      prev-syn (memq prev-syn '(2 3))
                      ;; current character is ;; end of buffer or not word.
                      (or (null cur-syn) (not (memq cur-syn '(2 3)))))
                          ;; call
                          (or (expand-abbrev) ; return t if was found
                              (completion-at-point) ; return t if was found
                              (condition-case err ; dabbrev-expand without errors
                                  (progn (dabbrev-expand arg)
                                         (message "dabbrev-expand succeeded")
                                         t)
                                (user-error (message "%s" (error-message-string err)) nil)
                                (error (message "dabbrev-expand failed: %s" (error-message-string err)) nil))))))))


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
  (seq-find (lambda(step)
                (message (symbol-name step)) ; debug
                (funcall step arg))
            indent-for-tab-steps))



(defun indent-region-like-first-line (start end)
  "Indent all lines like first.
Apply `indent-according-to-mode' to the first line.
And indent rigidly others."
  (deactivate-mark t)
  (save-excursion
    (goto-char start)
    (beginning-of-line)
    (let ((ciw (current-indentation))
          (cl (count-lines start end)))
      (indent-according-to-mode nil) ;; indent first line
      (when (> cl 1)
        (let ((differ (- (current-indentation) ciw) ) ; was = 1, become=4, 4-1 = 3+1 =4
              (end (save-excursion (forward-line (1- cl))
                                   (line-end-position))))
          (indent-rigidly (point) end differ))))))

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
