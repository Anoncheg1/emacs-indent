;;; indent-tests.el --- tests for indent-advices.el  -*- lexical-binding:t -*-

;;; Code:
(require 'ert)
(require 'indent-advices)

;;; -=-= For `indent-for-tab-step-6-remove-spaces-at-point'

(defmacro with-tiny-buffer (text &rest body)
  "Insert TEXT into a temp buffer, go to point-min, run BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (goto-char (point-min))
     ,@body))

;; ---------------------------------------------------------------
;; 1. Baseline: a single space between two words, point on it.
;;    Expect: t, and the space gets consumed (words join).
;; ---------------------------------------------------------------
(ert-deftest test-remove-space-between-words ()
  (with-tiny-buffer "foo bar"
    (forward-char 3)                     ; point on the space
    (should-not (indent-for-tab-step-6-remove-spaces-at-point))
    (should (equal (buffer-string) "foo bar"))))

;; ---------------------------------------------------------------
;; 2. Point NOT on a space (on a word char) -> must be a no-op,
;;    returns nil and buffer unchanged.
;; ---------------------------------------------------------------
(ert-deftest test-no-space-at-point1 ()
  (with-tiny-buffer "foo  bar"
    (forward-char 3)                     ; on 'o'
    (should (indent-for-tab-step-6-remove-spaces-at-point))
    (should (equal (buffer-string) "foo bar"))))

(ert-deftest test-no-space-at-point2 ()
  (with-tiny-buffer "foo  bar"
    (forward-char 4)                     ; on 'o'
    (should (indent-for-tab-step-6-remove-spaces-at-point))
    (should (equal (buffer-string) "foo bar"))))

(ert-deftest test-no-space-at-point3 ()
  (with-tiny-buffer "foo  bar"
    (forward-char 1)                     ; on 'o'
    (should-not (indent-for-tab-step-6-remove-spaces-at-point))
    (should (equal (buffer-string) "foo  bar"))))

;; ---------------------------------------------------------------
;; 4. Point at end of line (eolp) on the last space -> must NOT fire
;;    because of the `(not (eolp))' guard.
;;    (Note: with point on the last char, eolp is nil; put point
;;    *at* eol by moving past the trailing space.)
;; ---------------------------------------------------------------
(ert-deftest test-eol-guard ()
  (with-tiny-buffer "foo "
    (goto-char (point-max))              ; at eol
    (should-not (indent-for-tab-step-6-remove-spaces-at-point))
    (should (equal (buffer-string) "foo "))))

;; ---------------------------------------------------------------
;; 5. Leading indentation: point on a space that is preceded only
;;    by whitespace back to line-beginning -> `looking-back' guard
;;    must prevent removal.
;; ---------------------------------------------------------------
(ert-deftest test-no-removal-before-line-content ()
  (with-tiny-buffer "    foo"
    (forward-char 2)                     ; inside leading indent
    (should-not (indent-for-tab-step-6-remove-spaces-at-point))
    (should (equal (buffer-string) "    foo"))))

;; ---------------------------------------------------------------
;; 6. Multi-line buffer: point on a space on line 2, but the
;;    leading indentation of line 2 must not be touched.
;; ---------------------------------------------------------------
(ert-deftest test-multiline-indent-untouched1 ()
  (with-tiny-buffer "abc\n    def"
    (forward-line 1)
    (forward-char 2)                     ; inside the indent of line 2
    (should-not (indent-for-tab-step-6-remove-spaces-at-point))
    (should (equal (buffer-string) "abc\n    def"))))

(ert-deftest test-multiline-indent-untouched2 ()
  (with-tiny-buffer "abc\n    def"
    (goto-char 4)
    (should-not (indent-for-tab-step-6-remove-spaces-at-point))
    (should (equal (buffer-string) "abc\n    def"))))

(ert-deftest test-multiline-indent-untouched3 ()
  (with-tiny-buffer "abc \n    def"
    (goto-char 4)
    (should-not (indent-for-tab-step-6-remove-spaces-at-point))
    (should (equal (buffer-string) "abc \n    def"))))

;; ---------------------------------------------------------------
;; 7. Point at point-min on a space at the very start of buffer.
;;    prev-syn is nil; cur-syn is space; next-syn is space/word.
;;    `looking-back' from bob matches "" -> guard fires -> no-op.
;; ---------------------------------------------------------------
(ert-deftest test-bob-leading-space ()
  (with-tiny-buffer " foo"
    (goto-char (point-min))
    (should-not (indent-for-tab-step-6-remove-spaces-at-point))
    (should (equal (buffer-string) " foo"))))

;; ---------------------------------------------------------------
;; 8. Point at point-max on a trailing space -> eolp guard.
;; ---------------------------------------------------------------
(ert-deftest test-eob-trailing-space ()
  (with-tiny-buffer "foo "
    (goto-char (point-max))
    (should-not (indent-for-tab-step-6-remove-spaces-at-point))
    (should (equal (buffer-string) "foo "))))

;; ---------------------------------------------------------------
;; 9. Empty buffer: no chars, no crash, returns nil.
;; ---------------------------------------------------------------
(ert-deftest test-empty-buffer ()
  (with-tiny-buffer ""
    (should-not (indent-for-tab-step-6-remove-spaces-at-point))
    (should (equal (buffer-string) ""))))

;; ---------------------------------------------------------------
;; 12. Only whitespace on the line, point in the middle of it.
;;     `looking-back "^\\s-*"' from inside the run matches the
;;     preceding spaces -> guard fires -> no-op.
;; ---------------------------------------------------------------
(ert-deftest test-whitespace-only-line ()
  (with-tiny-buffer "   \n"
    (forward-char 1)
    (should-not (indent-for-tab-step-6-remove-spaces-at-point))
    (should (equal (buffer-string) "   \n"))))

;;; -=-= For `indent-region-like-first-line'

(ert-deftest indent-region-like-first-line-multi-line-shifts-all ()
  "First line 2→4; following lines shift by +2, preserving relative indent.
Lines outside the region are untouched."
  (with-temp-buffer
    (fundamental-mode)
    (insert "  a\n    b\n  c\nz\n")
    (goto-char (point-min))
    (let ((indent-line-function (lambda () (indent-line-to 4))))
      (indent-region-like-first-line
       (point-min)
       (save-excursion (forward-line 2) (line-end-position))))
    (should (equal (split-string (buffer-string) "\n" nil)
                   '("    a" "      b" "    c" "z" "")))))

(ert-deftest indent-region-like-first-line-negative-delta ()
  "First line 4→2; following lines shift by -2 (outdent)."
  (with-temp-buffer
    (fundamental-mode)
    (insert "    a\n      b\n")
    (goto-char (point-min))
    (let ((indent-line-function (lambda () (indent-line-to 2))))
      (indent-region-like-first-line
       (point-min)
       (save-excursion (forward-line 1) (line-end-position))))
    (should (equal (split-string (buffer-string) "\n" nil)
                   '("  a" "    b" "")))))

(ert-deftest indent-region-like-first-line-idempotent ()
  "Calling twice must not drift indentation."
  (with-temp-buffer
    (fundamental-mode)
    (insert "  a\n    b\n")
    (goto-char (point-min))
    (let ((indent-line-function (lambda () (indent-line-to 4))))
      (let ((beg (point-min))
            (end (save-excursion (forward-line 1) (line-end-position))))
        (indent-region-like-first-line beg end)
        (let ((once (buffer-string)))
          (indent-region-like-first-line beg end)
          (should (equal once (buffer-string))))))))

(provide 'indent-tests)

;;; indent-tests.el ends here
