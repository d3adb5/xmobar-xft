;; xmobar.el --- Display xmobar text output -*- lexical-binding: t -*-

;; Copyright 2022 jao <jao@gnu.org>
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: unix

;; Heavily inspired by Steven Allen's https://github.com/Stebalien/i3bar.el

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Displays the output of an xmobar command in the Emacs mode-line (or tab-line).

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'tab-bar)
(require 'xterm-color nil t)

(defgroup xmobar nil
  "xmobar status display for Emacs."
  :version "0.0.1"
  :group 'mode-line)

(defcustom xmobar-command '("xmobar" "-TAnsi")
  "The xmobar command and flags."
  :type '(choice (string :tag "Shell Command")
                 (repeat (string))))

(defcustom xmobar-tab-bar t
  "Whether to dispaly xmobar output in the tab bar."
  :type 'boolean)

(defcustom xmobar-tab-split nil
  "Split on this string for `xmobar-left-string' and `xmobar-right-string'."
  :type 'string)

(defcustom xmobar-tab-bar-format
  '(xmobar-left-string tab-bar-format-align-right xmobar-right-string)
  "Format for the tab bar when `xmobar-tab-bar' is t."
  :type 'list)

(defvar xmobar--process nil
  "The running xmobar process, if any.")

(defvar xmobar--left-string "")

(defvar xmobar-string ""
  "The xmobar string to be displayed in the mode-line or tab-bar.")

(put 'xmobar-string 'risky-local-variable t)

(defvar xmobar--colorize-fn
  (if (featurep 'xterm-color) #'xterm-color-filter #'ansi-color-apply))

(defvar xmobar--old-tab-format tab-bar-format)

(defun xmobar-string () xmobar-string)
(defun xmobar-right-string () xmobar-string)
(defun xmobar-left-string () xmobar--left-string)

;;;###autoload
(define-minor-mode xmobar-mode
  "Display an xmobar in the mode-line."
  :global t :group 'xmobar
  (xmobar--stop)
  (if xmobar-mode
      (progn (if xmobar-tab-bar
                 (progn
                   (setq xmobar--old-tab-format tab-bar-format)
                   (setq tab-bar-format xmobar-tab-bar-format)
                   (tab-bar-mode 1))
               (or global-mode-string (setq global-mode-string '("")))
               (unless (memq 'xmobar-string global-mode-string)
                 (setq global-mode-string
                       (append global-mode-string '(xmobar-string)))))
             (xmobar--start))
    (when xmobar-tab-bar (setq tab-bar-format xmobar--old-tab-format))))

(defun xmobar--update (update)
  "Apply an UPDATE to the xmobar bar."
  (when xmobar-mode
    (let* ((str (funcall xmobar--colorize-fn update))
           (strs (and xmobar-tab-split (split-string str xmobar-tab-split))))
      (setq xmobar-string (if strs (cadr strs) str)
            xmobar--left-string (or (car strs) "")))
    (force-mode-line-update t)))

(defun xmobar--process-filter (proc string)
  "Process output from the xmobar process."
  (let ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        ;; Write the input to the buffer (might be partial).
        (save-excursion
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (when (string-match-p "\n$" string)
          (xmobar--update (buffer-string))
          (delete-region (point-min) (point-max)))))))

(defun xmobar--process-sentinel (proc status)
  "Handle events from the xmobar process (PROC).
If the process has exited, this function stores the exit STATUS in
`xmobar-string'."
  (unless (process-live-p proc)
    (setq xmobar--process nil)
    (let ((buf (process-buffer proc)))
      (when (and buf (buffer-live-p buf)) (kill-buffer buf)))
    (setq xmobar-string (format "xmobar: %s" status) xmobar--left-string "")))

(defun xmobar--start ()
  "Start xmobar."
  (xmobar--stop)
  (condition-case err
      (setq xmobar--process
            (make-process
             :name "xmobar"
             :buffer " *xmobar process*"
             :stderr " *xmobar stderr*"
             :command (ensure-list xmobar-command)
             :connection-type 'pipe
             :noquery t
             :sentinel #'xmobar--process-sentinel
             :filter #'xmobar--process-filter))
    (error
     (setq xmobar-string
           (format "starting xmobar: %s" (error-message-string err))
           xmobar--left-string ""))))

(defun xmobar--stop ()
  "Stop xmobar."
  (when (and xmobar--process (process-live-p xmobar--process))
    (delete-process xmobar--process))
  (setq xmobar-string "" xmobar--left-string ""))

;;;###autoload
(defun xmobar-restart ()
  "Restart the xmobar program."
  (interactive)
  (unless xmobar-mode (user-error "The xmobar-mode is not enabled"))
  (xmobar--start))

(provide 'xmobar)
;;; xmobar.el ends here
