;;; pomodoro --- A timer for the Pomodoro Technique
;;;   (http://www.pomodorotechnique.com)

;; Copyright (C) 2012 Rodrigo Lazo (rlazo)

;; Author: Rodrigo Lazo <rlazo.paz@gmail.com>
;; Edited: 25 mar 2012

;; Inspired on the ideas from Dave Kerschner's (docgnome) pomodoro.el
;; which you can get from https://github.com/docgnome/pomodoro.el.
;; This implementation is different from Dave's, but it started as a
;; fork of pomodoro.el. Eventually I replaced most of the code with my
;; own.

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

;;; Code:

(defgroup pomodoro nil
  "Timer for the Pomodoro Technique in emacs"
  :prefix "pomodoro-"
  :group 'tools)

(defcustom pomodoro-nth-for-longer-break 4
  "Number of pomodoros before a long break"
  :group 'pomodoro
  :type 'integer)

(defcustom pomodoro-work-cycle
  '(:length 25
    :start-message "Let's go, do a pomodoro!"
    :name "Pomodoro"
    :mode-line-string "w")
  "Characteristics of a pomodoro work cycle"
  :group 'pomodoro
  :type 'plist)

(defcustom pomodoro-log-buffer "*pomodoros*"
  "Buffer name where to store the pomodoro log."
  :group 'pomodoro
  :type 'string)

(defcustom pomodoro-break-cycle
  '(:length 5
    :start-message "Time to take a break!"
    :name "Break"
    :mode-line-string "b")
  "Characteristics of a pomodoro break cycle"
  :group 'pomodoro
  :type 'plist)

(defcustom pomodoro-long-break-cycle
  '(:length 15
    :start-message "Well done! you earned a longer break."
    :name "Long Break"
    :mode-line-string "B")
  "Characteristics of a pomodoro long break cycle"
  :group 'pomodoro
  :type 'plist)

(defvar pomodoro-timer nil)
(defvar pomodoro-pomodoros 0)
(defvar pomodor-mode-line-string "")
(defvar pomodoro-current-cycle nil)
(defvar pomodoro-cycle-end-time nil
  "Time, as returned by current-time, when the current pomodoro
  cycle will end.")
(defvar pomodoro-cycle-start-time nil
  "Time, as returned by current-time, when the current cycle started.")

(defun pomodoro--log (&rest args)
  (save-current-buffer
    (with-current-buffer (get-buffer-create pomodoro-log-buffer)
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format-time-string "%D %H:%M" (current-time)) " - ")
        (apply 'insert args)
        (insert "\n")))))

(defun pomodoro-get-next-cycle (current-cycle)
  "Computes, based on current cycle, the next cycle, and returns it."
  (cond
   ((not (eq current-cycle pomodoro-work-cycle))
    pomodoro-work-cycle)
   ((and (> pomodoro-pomodoros 0)
         (= (mod pomodoro-pomodoros pomodoro-nth-for-longer-break) 0))
    pomodoro-long-break-cycle)
   (t
    pomodoro-break-cycle)))

(defun pomodoro-begin-cycle (cycle)
  (let ((now (current-time)))
    (pomodoro--log "Beginning " (plist-get cycle :name))
    (setq pomodoro-start-time now)
    (setq pomodoro-cycle-end-time
          (time-add now (seconds-to-time (* 60 (plist-get cycle :length)))))
    (setq pomodoro-current-cycle cycle)))

(defun pomodoro-compute-mode-line-string ()
  (format "%s %s[%d]" (plist-get pomodoro-current-cycle :mode-line-string)
          (format-time-string "%M:%S" (time-subtract pomodoro-cycle-end-time
                                                     (current-time)))
          pomodoro-pomodoros))

(defun pomodoro-tick ()
  (when (not (time-less-p (current-time) pomodoro-cycle-end-time))
    (pomodoro--log "Finished " (plist-get pomodoro-current-cycle :name))
    (cancel-timer pomodoro-timer)
    (when (eq pomodoro-current-cycle pomodoro-work-cycle)
      (setq pomodoro-pomodoros (1+ pomodoro-pomodoros))
      (pomodoro--log "[COUNT] "(int-to-string pomodoro-pomodoros) " done."))
    (let ((next-cycle (pomodoro-get-next-cycle pomodoro-current-cycle)))
      (message-box (plist-get next-cycle :start-message))
      (pomodoro-begin-cycle next-cycle)
      (setq pomodoro-timer (run-with-timer 0 1 'pomodoro-tick))))
  (setq pomodoro-mode-line-string (pomodoro-compute-mode-line-string))
  (force-mode-line-update))

(defun pomodoro-start ()
  (interactive)
  (pomodoro--log "[START POMODORO]")
  (pomodoro-begin-cycle pomodoro-work-cycle)
  (setq pomodoro-timer (run-with-timer 0 1 'pomodoro-tick)))

(defun pomodoro-stop ()
  (interactive)
  (pomodoro--log "[STOP POMODORO]")
  (cancel-timer pomodoro-timer)
  (setq pomodoro-mode-line-string ""
        pomodoro-current-cycle nil)
  (force-mode-line-update))

(defun pomodoro-restart ()
  (interactive)
  (pomodoro--log "[RESTART POMODORO]")
  (pomodoro-stop)
  (pomodoro-start))

(defun pomodoro-reset ()
  (interactive)
  (pomodoro--log "[RESET POMODODO]")
  (pomodoro-stop)
  (setq pomodoro-pomodoros 0))

(setq-default mode-line-format (cons mode-line-format '(pomodoro-mode-line-string)))

(provide 'pomodoro)
