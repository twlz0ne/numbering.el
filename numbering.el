;;; numbering.el --- Numbering mode -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2019/02/18
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/twlz0ne/crease
;; Keywords: rectangle

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

;; Provide features like rectangle-number-lines but more interactive.

;;; Change Log:

;;  0.1.0  2019/02/18  Initial version.

;;; Code:

(defcustom numbering-format-leading-zerop nil
  "Display leading zero for numbers."
  :group 'crease
  :type 'bealoon)

(defcustom numbering-default-format "%d"
  "Default number format."
  :group 'crease
  :type 'string)

(defcustom numbering-default-start-at 1
  "Default number start from."
  :group 'crease
  :type 'integer)

(defvar numbering--format numbering-default-format)

(defvar numbering--start-at numbering-default-start-at)

(defvar numbering--last-position nil)

(defun numbering--read-args ()
  "Return list of start-at and format."
  (let* ((prompt (format "Number to count from (default %s →  \"%s\"): " numbering--start-at numbering--format))
         (default numbering--start-at)
         (num (read-from-minibuffer
               prompt nil nil nil nil
               (when default
                 (if (consp default)
                     (mapcar 'number-to-string (delq nil default))
                   (number-to-string default))))))
    (cond ((zerop (length num)) (list default numbering--format))
          ((stringp num)
           (list (read num)
                 (let ((rbeg (if (region-active-p) (region-beginning) (point)))
                       (rend (if (region-active-p) (region-end) (point))))
                   (read-string
                    "Format string: "
                    (if (region-active-p)
                        (concat "%"
                                (and numbering-format-leading-zerop "0")
                                (number-to-string (abs (- rbeg rend))) "d")
                      numbering--format))))))))

(defun numbering--line-move (n &optional beg end)
  (let ((col1 (- (or beg (point)) (point-at-bol)))
        (col2 (- (or end (point)) (point-at-bol))))
    (forward-line n)
    (save-restriction
      (narrow-to-region (point-at-bol) (point-at-eol))
      (if (region-active-p)
          (progn
            (move-to-column (max col1 col2) t)
            (set-mark (point))
            (move-to-column (min col1 col2))
            (setq deactivate-mark nil))
        (move-to-column (min col1 col2) t)))))

(defun numbering-apply (&optional start-at format)
  "Insert number at point or replace the text of region.

START-AT, if non-nil, should be a number from which to begin
counting.  FORMAT, if non-nil, should be a format string to pass
to `format' along with the line count."
  (interactive)
  (let* ((rbeg (if (region-active-p) (region-beginning) (point)))
         (rend (if (region-active-p) (region-end) (point)))
         (start-at (or start-at (read-number "Number to count from: " 1)))
         (format (or format
                     (read-string
                      "Format string: "
                      (if (region-active-p)
                          (concat "%"
                                  (and numbering-format-leading-zerop "0")
                                  (number-to-string (abs (- rbeg rend)))
                                  "d")
                        numbering--format))))
         (numstr (format format start-at)))
    (if (region-active-p)
        (delete-extract-rectangle rbeg rend))
    (insert numstr)
    (if (region-active-p)
        (progn
          (goto-char rbeg)
          (set-mark rend)
          (setq deactivate-mark nil)
          (setq crase--last-position (if (region-active-p) (cons rbeg rend) rbeg)))
      (backward-char (length numstr)))
    (setq numbering--start-at start-at)
    (setq numbering--format format)
    ))

(defun numbering-increase-curr ()
  "Increase number at point which generated recently."
  (interactive)
  (unless (region-active-p)
    (let* ((numstr (format numbering--format numbering--start-at)))
      (delete-rectangle (point) (+ (point) (length numstr)))))
  (numbering-apply (1+ numbering--start-at) numbering--format))

(defun numbering-decrease-curr ()
  "Decrease number at point which generated recently."
  (interactive)
  (unless (region-active-p)
    (let* ((numstr (format numbering--format numbering--start-at)))
      (delete-rectangle (point) (+ (point) (length numstr)))))
  (numbering-apply (1- numbering--start-at) numbering--format))

(defun numbering-increase-next ()
  "Inset increased number to next line."
  (interactive)
  (numbering-next-line)
  (numbering-apply (1+ numbering--start-at) numbering--format))

(defun numbering-increase-prev ()
  "Inset increased number to prev line."
  (interactive)
  (numbering-prev-line)
  (numbering-apply (1+ numbering--start-at) numbering--format))

(defalias 'numbering-skip-next 'numbering-next-line)

(defun numbering-next-line ()
  "Move to next line without increase/decrease number."
  (interactive)
  (apply 'numbering--line-move 1 (and (region-active-p)
                                   (list (region-beginning)
                                         (region-end)))))

(defalias 'numbering-skip-prev 'numbering-prev-line)

(defun numbering-prev-line ()
  "Move to prev line without increase/decrease number."
  (interactive)
  (apply 'numbering--line-move -1 (and (region-active-p)
                                    (list (region-beginning)
                                          (region-end)))))

(defvar numbering-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-n") 'numbering-increase-next)
    (define-key map (kbd "C-p") 'numbering-increase-prev)
    (define-key map (kbd "C-g") 'numbering-quit)
    map)
  "Keymap for `numbering-mode'.")

(define-minor-mode numbering-mode  
  "Toggle Numbering mode."
  :init-value nil
  :lighter " Numbering"
  :keymap numbering-mode-map
  )

;;;###autoload
(defun numbering-start ()
  "Active numbering-mode with new start number and format."
  (interactive)
  (numbering-mode 1)
  (funcall 'numbering-apply))

;;;###autoload
(defun numbering-resume ()
  "Active numbering-mode with last start number and format."
  (interactive)
  (numbering-mode 1)
  (apply 'numbering-apply (numbering--read-args)))

;;;###autoload
(defun numbering-quit ()
  "Deactive numbering-mode."
  (interactive)
  (numbering-mode 0)
  (when (region-active-p)
    (deactivate-mark)))

(provide 'numbering)

;;; numbering.el ends here
