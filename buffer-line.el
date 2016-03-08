;;; buffer-line.el --- Tab like for Emacs.

;; Copyright (C) 2016 Quang Linh LE

;; Author: Quang Linh LE <linktohack@gmail.com>
;; URL: http://github.com/linktohack/buffer-line
;; Version: 1.0.0
;; Keywords: tab buffer line

;; This file is not part of GNU Emacs.

;;; License:

;; This file is part of buffer-line
;;
;; buffer-line is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; buffer-line is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This program display a tab-like buffers (buffer-line) in echo area.
;; It aslo provides some default comands and key bindings to switch to
;; next and previous normal buffer.

;;; Example:
;;
;; (use-package buffer-line
;;   :load-path "~/Dropbox/ProjectWeekends/lisp/buffer-line"
;;   :init
;;   (bind-key "gb" #'buffer-line/next-buffer evil-motion-state-map))
;;
;; `s-}' and `s-{' 'to go to next and previous buffer.
;; `3s-}' or `3gb' to go to 3th buffer (see above for `evil' binding)


;;; Code:

(defcustom buffer-line-idle-delay 0.5
  "Timer delay.")

(defcustom buffer-line-special-buffer-regexp '("\\*scratch\\*")
  "Special buffer to display.")

(defcustom buffer-line-place 'echo-area
  "Place to put buffer-line.

Acceptable value: `nil' or `echo-area', `mode-line'.")

(defcustom buffer-line-mode-line-limit 40
  "Limit values for `mode-line'.")

(defcustom buffer-line-include-current-buffer nil
  "Should `buffer-line' include the current buffer.")

(defvar buffer-line--timer nil
  "Timer variable.")

(defvar buffer-line--saved-lighter nil
  "Saved lighter.")

(defun buffer-line/list ()
  "List of normal buffers."
  (if (not (minibufferp))
      (let ((current (buffer-name))
            next all)
        (when buffer-line-include-current-buffer
          (push current all))
        (while
            (progn
              (next-buffer)
              (setq next (buffer-name))
              (when (and (not (string= next current))
                         (buffer-line/normalp next))
                (push next all))
              (not (string= next current))))
        (nreverse all))
    (unwind-protect
        (save-window-excursion
          (other-window 1)
          (buffer-line/list)))))

(defun buffer-line/string (&optional limit)
  "String representation of buffer-line's list."
  (let* ((list (cl-loop for buff in (buffer-line/list)
                        for index from (if buffer-line-include-current-buffer 0 1)
                        collect (format "%d: %s" index buff)))
         (line (mapconcat #'identity list " | ")))
    (if (and limit
             (< limit (length line)))
        (format "%s..." (substring line 0 (- limit 3)))
      line)))

(defun buffer-line/normalp (name)
  "Whether a buffer is normal (.i.e. not special.)"
  (let (normalp)
    (dolist (special buffer-line-special-buffer-regexp)
      (when (string-match special name)
        (setq normalp t)))
    (unless (string-match "^\\*" name)
      (setq normalp t))
    normalp))

(defun buffer-line/next-buffer (&optional count)
  "Next normal buffer."
  (interactive "p")
  (let ((list (buffer-line/list))
        len)
    (unless buffer-line-include-current-buffer
      (push (buffer-name) list))
    (setq len (length list))
    (unless (<= len 1)
      (switch-to-buffer (nth (mod (or count 1) len) list))
      (buffer-line/show))))

(defun buffer-line/previous-buffer (&optional count)
  "Previous normal buffer."
  (interactive "p")
  (buffer-line/next-buffer (- (or count 1))))

(defun buffer-line/show (&optional place)
  "Show buffer line."
  (setq place (or place buffer-line-place))
  (unless (or cursor-in-echo-area
              (active-minibuffer-window))
    (cond
     ;; mode-line
     ((eq place 'mode-line)
      (let ((mode (assq 'buffer-line-mode minor-mode-alist)))
        (unless buffer-line--saved-lighter
          (setq buffer-line--saved-lighter (cdr mode)))
        (setcdr mode `(,(concat " ["
                                (buffer-line/string buffer-line-mode-line-limit)
                                "]")))))
     ;; echo-area
     ((eq place 'echo-area)
      (when buffer-line--saved-lighter
        (let ((mode (assq 'buffer-line-mode minor-mode-alist)))
          (setcdr mode buffer-line--saved-lighter)
          (setq buffer-line--saved-lighter nil)))
      (let ((message-log-max nil))
        (message (buffer-line/string (frame-width))))))))

(defun buffer-line/schedule-timer ()
  (interactive)
  (unless buffer-line--timer
    (buffer-line/show)
    (setq buffer-line--timer
          (run-with-idle-timer
           buffer-line-idle-delay t
           #'buffer-line/show))))

(define-minor-mode buffer-line-mode
  "Display normal buffers in echo area, switch to one of which in order."
  :lighter " bl"
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "s-}") #'buffer-line/next-buffer)
            (define-key map (kbd "s-{") #'buffer-line/previous-buffer)
            map)
  (if buffer-line-mode
      (buffer-line/schedule-timer)
    (cancel-timer buffer-line--timer)
    (setq buffer-line--timer nil)))

(provide 'buffer-line)
