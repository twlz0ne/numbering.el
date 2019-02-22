;;; test-numbering.el --- Test numbering -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Gong Qijian <gongqijian@gmail.com>

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

;;; Code:

(require 'ert)
(require 'numbering)

(unless (version< "24.5" emacs-version)
  (defun line-move@correct-point (fn arg &optional noerror to-end try-vscroll)
    (let ((col (- (point) (point-at-bol))))
      (funcall fn arg noerror to-end try-vscroll)
      (unless (zerop col)
        (goto-char (+ col (point-at-bol))))))
  (advice-add 'line-move :around 'line-move@correct-point))

;;; Test line move

(defun test-numbering--mark-next (n)
  "Forward N line, return `(cons region-bound . region-text)'."
  (cond ((> n 0) (call-interactively 'numbering-next-line))
        ((< n 0) (call-interactively 'numbering-prev-line)))
  (let* ((beg (if (region-active-p) (region-beginning) (point)))
         (end (if (region-active-p) (region-end)       (point)))
         (mark (cons (cons beg end) (buffer-substring-no-properties beg end))))
    mark))

(ert-deftest test-numbering-next-line-1 ()
  (with-temp-buffer
    (insert "foo\nbar\nqux")
    (goto-char 1)
    (should (equal (test-numbering--mark-next 0)  '((1 . 1) . "")))
    (should (equal (test-numbering--mark-next 1)  '((5 . 5) . "")))
    (should (equal (test-numbering--mark-next 1)  '((9 . 9) . "")))
    (should (equal (test-numbering--mark-next -1) '((5 . 5) . "")))
    (should (equal (test-numbering--mark-next -1) '((1 . 1) . "")))
    ))

(ert-deftest test-numbering-next-line-2 ()
  (with-temp-buffer
    (insert "foo\nbar\nqux")
    (goto-char 2)
    (should (equal (test-numbering--mark-next 0)  '((2  . 2 ) . "")))
    (should (equal (test-numbering--mark-next 1)  '((6  . 6 ) . "")))
    (should (equal (test-numbering--mark-next 1)  '((10 . 10) . "")))
    (should (equal (test-numbering--mark-next -1) '((6  . 6 ) . "")))
    (should (equal (test-numbering--mark-next -1) '((2  . 2 ) . "")))
    ))

(ert-deftest test-numbering-next-line-3 ()
  (with-temp-buffer
    (insert "foo\nbar\nqux")
    (goto-char 3)
    (should (equal (test-numbering--mark-next 0)  '((3  . 3 ) . "")))
    (should (equal (test-numbering--mark-next 1)  '((7  . 7 ) . "")))
    (should (equal (test-numbering--mark-next 1)  '((11 . 11) . "")))
    (should (equal (test-numbering--mark-next -1) '((7  . 7 ) . "")))
    (should (equal (test-numbering--mark-next -1) '((3  . 3 ) . "")))
    ))

(ert-deftest test-numbering-next-line-4 ()
  (with-temp-buffer
    (insert "foo\nbar\nqux")
    (goto-char 4)
    (should (equal (test-numbering--mark-next 0)  '((4  . 4 ) . "")))
    (should (equal (test-numbering--mark-next 1)  '((8  . 8 ) . "")))
    (should (equal (test-numbering--mark-next 1)  '((12 . 12) . "")))
    (should (equal (test-numbering--mark-next -1) '((8  . 8 ) . "")))
    (should (equal (test-numbering--mark-next -1) '((4  . 4 ) . "")))
    ))

(ert-deftest test-numbering-next-line-1-4 ()
  (with-temp-buffer
    (insert "foo\nbar\nqux")
    (set-mark 1)
    (goto-char 4)
    (when noninteractive
      (transient-mark-mode))
    (should (equal (test-numbering--mark-next 0)  '((1 . 4 ) . "foo")))
    (should (equal (test-numbering--mark-next 1)  '((5 . 8 ) . "bar")))
    (should (equal (test-numbering--mark-next 1)  '((9 . 12) . "qux")))
    (should (equal (test-numbering--mark-next -1) '((5 . 8 ) . "bar")))
    (should (equal (test-numbering--mark-next -1) '((1 . 4 ) . "foo")))
    ))

;;; Test number increase

(defun test-numbering--increase-next (n start-at format)
  "Forward N line and applay, return buffer string."
  (cond ((> n 0) (call-interactively 'numbering-next-line))
        ((< n 0) (call-interactively 'numbering-prev-line)))
  (numbering-apply start-at format)
  (buffer-substring-no-properties (point-min) (point-max)))

(ert-deftest test-numbering-increase-1 ()
  (with-temp-buffer
    (insert "foo\nbar\nqux")
    (goto-char 1)
    (when noninteractive
      (transient-mark-mode))
    (should (equal (test-numbering--increase-next  0 1 "%d") "1foo\nbar\nqux"))
    (should (equal (test-numbering--increase-next  1 2 "%d") "1foo\n2bar\nqux"))
    (should (equal (test-numbering--increase-next  1 3 "%d") "1foo\n2bar\n3qux"))
    (should (equal (test-numbering--increase-next -1 4 "%d") "1foo\n42bar\n3qux"))
    (should (equal (test-numbering--increase-next -1 5 "%d") "51foo\n42bar\n3qux"))
    ))

(ert-deftest test-numbering-increase-2 ()
  (with-temp-buffer
    (insert "foo\nbar\nqux")
    (goto-char 2)
    (when noninteractive
      (transient-mark-mode))
    (should (equal (test-numbering--increase-next  0 1 "%d") "f1oo\nbar\nqux"))
    (should (equal (test-numbering--increase-next  1 2 "%d") "f1oo\nb2ar\nqux"))
    (should (equal (test-numbering--increase-next  1 3 "%d") "f1oo\nb2ar\nq3ux"))
    (should (equal (test-numbering--increase-next -1 4 "%d") "f1oo\nb42ar\nq3ux"))
    (should (equal (test-numbering--increase-next -1 5 "%d") "f51oo\nb42ar\nq3ux"))
    ))

(ert-deftest test-numbering-increase-3 ()
  (with-temp-buffer
    (insert "foo\nbar\nqux")
    (goto-char 3)
    (when noninteractive
      (transient-mark-mode))
    (should (equal (test-numbering--increase-next  0 1 "%d") "fo1o\nbar\nqux"))
    (should (equal (test-numbering--increase-next  1 2 "%d") "fo1o\nba2r\nqux"))
    (should (equal (test-numbering--increase-next  1 3 "%d") "fo1o\nba2r\nqu3x"))
    (should (equal (test-numbering--increase-next -1 4 "%d") "fo1o\nba42r\nqu3x"))
    (should (equal (test-numbering--increase-next -1 5 "%d") "fo51o\nba42r\nqu3x"))
    ))

(ert-deftest test-numbering-increase-4 ()
  (with-temp-buffer
    (insert "foo\nbar\nqux")
    (goto-char 4)
    (when noninteractive
      (transient-mark-mode))
    (should (equal (test-numbering--increase-next  0 1 "%d") "foo1\nbar\nqux"))
    (should (equal (test-numbering--increase-next  1 2 "%d") "foo1\nbar2\nqux"))
    (should (equal (test-numbering--increase-next  1 3 "%d") "foo1\nbar2\nqux3"))
    (should (equal (test-numbering--increase-next -1 4 "%d") "foo1\nbar42\nqux3"))
    (should (equal (test-numbering--increase-next -1 5 "%d") "foo51\nbar42\nqux3"))
    ))

(ert-deftest test-numbering-increase-1-4 ()
  (with-temp-buffer
    (insert "foo\nbar\nqux")
    (set-mark 1)
    (goto-char 4)
    (when noninteractive
      (transient-mark-mode))
    (should (equal (test-numbering--increase-next  0 1 "%03d") "001\nbar\nqux"))
    (should (equal (test-numbering--increase-next  1 2 "%03d") "001\n002\nqux"))
    (should (equal (test-numbering--increase-next  1 3 "%03d") "001\n002\n003"))
    (should (equal (test-numbering--increase-next -1 4 "%03d") "001\n004\n003"))
    (should (equal (test-numbering--increase-next -1 5 "%03d") "005\n004\n003"))
    ))

;;; Test number increase with blank lines

(ert-deftest test-numbering-increase-with-blank-line-1 ()
  (with-temp-buffer
    (insert "foo\n\nqux")
    (goto-char 1)
    (when noninteractive
      (transient-mark-mode))
    (should (equal (test-numbering--increase-next  0 1 "%d") "1foo\n\nqux"))
    (should (equal (test-numbering--increase-next  1 2 "%d") "1foo\n2\nqux"))
    (should (equal (test-numbering--increase-next  1 3 "%d") "1foo\n2\n3qux"))
    (should (equal (test-numbering--increase-next -1 4 "%d") "1foo\n42\n3qux"))
    (should (equal (test-numbering--increase-next -1 5 "%d") "51foo\n42\n3qux"))
    )
  )

(ert-deftest test-numbering-increase-with-blank-line-2 ()
  (with-temp-buffer
    (insert "foo\n\nqux")
    (goto-char 2)
    (when noninteractive
      (transient-mark-mode))
    (should (equal (test-numbering--increase-next  0 1 "%d") "f1oo\n\nqux"))
    (should (equal (test-numbering--increase-next  1 2 "%d") "f1oo\n 2\nqux"))
    (should (equal (test-numbering--increase-next  1 3 "%d") "f1oo\n 2\nq3ux"))
    (should (equal (test-numbering--increase-next -1 4 "%d") "f1oo\n 42\nq3ux"))
    (should (equal (test-numbering--increase-next -1 5 "%d") "f51oo\n 42\nq3ux"))
    )
  )

(ert-deftest test-numbering-increase-with-blank-line-3 ()
  (with-temp-buffer
    (insert "foo\n\nqux")
    (goto-char 3)
    (when noninteractive
      (transient-mark-mode))
    (should (equal (test-numbering--increase-next  0 1 "%d") "fo1o\n\nqux"))
    (should (equal (test-numbering--increase-next  1 2 "%d") "fo1o\n  2\nqux"))
    (should (equal (test-numbering--increase-next  1 3 "%d") "fo1o\n  2\nqu3x"))
    (should (equal (test-numbering--increase-next -1 4 "%d") "fo1o\n  42\nqu3x"))
    (should (equal (test-numbering--increase-next -1 5 "%d") "fo51o\n  42\nqu3x"))
    )
  )

(ert-deftest test-numbering-increase-with-blank-line-4 ()
  (with-temp-buffer
    (insert "foo\n\nqux")
    (goto-char 4)
    (when noninteractive
      (transient-mark-mode))
    (should (equal (test-numbering--increase-next  0 1 "%d") "foo1\n\nqux"))
    (should (equal (test-numbering--increase-next  1 2 "%d") "foo1\n   2\nqux"))
    (should (equal (test-numbering--increase-next  1 3 "%d") "foo1\n   2\nqux3"))
    (should (equal (test-numbering--increase-next -1 4 "%d") "foo1\n   42\nqux3"))
    (should (equal (test-numbering--increase-next -1 5 "%d") "foo51\n   42\nqux3"))
    )
  )

(ert-deftest test-numbering-increase-with-blank-line-1-4 ()
  (with-temp-buffer
    (insert "foo\n\nqux")
    (goto-char 1)
    (set-mark 4)
    (when noninteractive
      (transient-mark-mode))
    (should (equal (test-numbering--increase-next  0 1 "%03d") "001\n\nqux"))
    (should (equal (test-numbering--increase-next  1 2 "%03d") "001\n002\nqux"))
    (should (equal (test-numbering--increase-next  1 3 "%03d") "001\n002\n003"))
    (should (equal (test-numbering--increase-next -1 4 "%03d") "001\n004\n003"))
    (should (equal (test-numbering--increase-next -1 5 "%03d") "005\n004\n003"))
    )
  )

(provide 'test-numbering)

;;; test-numbering.el ends here
