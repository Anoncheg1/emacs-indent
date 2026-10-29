;;; indent-tests.el --- tests for indent.el

;;; Code:
(require 'ert)
(require 'indent-advices)

;; -=-= For `indent-for-tab-step-6-remove-spaces-at-point'

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

(provide 'indent-tests)

;;; indent-tests.el ends here
