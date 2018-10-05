;;; dctrl-adb.el --- Device control ADB backend

;; Copyright (C) 2014-2018 Free Software Foundation, Inc.

;; Author: Jeremy Compostella <jeremy.compostella@gmail.com>

;; Keywords: comm, processes, devices
;; Package: device-control

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is a `device-control' backend for Android Debug
;; Bridge.

(require 'device-control)

(defvar adb-exec "adb")

(defvar adb-push-file-history '())

(defun dctrl-adb-run (&rest args)
  (dctrl-run-process
   (nconc (list adb-exec) (if dctrl-automatic-mode
			      '()
			    (list "-s" dctrl-device-name))
	  args)))

(defun dctrl-adb-action-reboot ()
  (dctrl-adb-run "reboot"))

(defun dctrl-adb-action-root ()
  (dctrl-adb-run "root"))

(defun dctrl-adb-action-remount ()
  (dctrl-adb-run "remount"))

(defun dctrl-adb-action-disable-verity ()
  (dctrl-adb-run "disable-verity"))

(defvar dctrl-adb-reboot-target '("" "bootloader" "recovery"))

(defun dctrl-adb-action-reboot (&optional target)
  (let ((target (or target (ido-completing-read "Target: " dctrl-adb-reboot-target nil t))))
    (dctrl-adb-run "reboot" target)))

(defun dctrl-adb-action-shutdown ()
  (dctrl-adb-run "shell" "am" "start" "-a"
		 "android.intent.action.ACTION_REQUEST_SHUTDOWN"))

(defun dctrl-adb-action-push-file (&optional filename dst-filename)
  (let* ((ido-file-history 'adb-push-file-history)
	 (src (expand-file-name (or filename (ido-read-file-name "Source: " ))))
	 (dst (or dst-filename (read-string "Target: " nil))))
    (with-untramped-file src
      (dctrl-adb-run "push" src dst))))

(defun dctrl-adb-aosp-out-dir ()
  (when (and aosp-path aosp-board-name)
    (concat aosp-path "/out/target/product/" aosp-board-name "/")))

(defun dctrl-adb-action-ota (&optional file)
  (let ((file file))
    (unless (and file (file-exists-p file))
      (setq file (ido-read-file-name "File to flash: " (dctrl-adb-aosp-out-dir))))
    (with-untramped-file file
      (append (dctrl-adb-run "root")
	      (dctrl-action-wait 2)
	      (dctrl-adb-run "push" file "/data/local/tmp/update.zip")
	      (dctrl-adb-run "shell" "mkdir" "-p" "/cache/recovery")
	      (dctrl-adb-run "shell" "echo '--update_package=/data/local/tmp/update.zip' > /cache/recovery/command")
	      (dctrl-adb-run "shell" "start" "pre-recovery")))))

(defun dctrl-adb-connected-p ()
  (let ((devices (dctrl-adb-guess-device-names)))
    (if dctrl-automatic-mode
	devices
      (find dctrl-device-name devices :test 'string=))))

(defun dctrl-adb-get-actions ()
  (dctrl-build-fun-list "dctrl-adb-action-"
			(if (dctrl-adb-connected-p) 'success 'error)))

(defconst adb-dev-line "^\\\([[:alnum:]]+\\\)[[:space:]]+device$")

(defun dctrl-adb-guess-device-names ()
  (delq nil (mapcar (lambda (line)
		      (when (numberp (string-match adb-dev-line line))
			(match-string 1 line)))
		    (split-string (shell-command-to-string
				   (concat adb-exec " devices")) "\n" t))))

(dctrl-register-backend
 (make-dctrl-backend :name "adb"
		     :get-actions 'dctrl-adb-get-actions
		     :guess-device-names 'dctrl-adb-guess-device-names))

(provide 'dctrl-adb)
