;;; dctrl-fastboot.el --- Device control Fastboot backend

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

;; This package is a `device-control' backend for Android Fastboot.

(require 'device-control)

(defcustom fastboot-program (purecopy "fastboot")
  "The default fastboot program."
  :group 'device-control)

(defvar aosp-path nil)
(defvar aosp-board-name nil)

(defun dctrl-fsb-run (&rest args)
  (dctrl-run-process
   (nconc (list fastboot-program) (if dctrl-automatic-mode
				      '()
				    (list "-p" dctrl-device-name))
	  args)))

(defcustom dctrl-fsb-targets
  '(("boot"		.	"boot.img")
    ("bootloader"	.	"bootloader.img")
    ("recovery"		.	"recovery.img")
    ("system"		.	"system.img")
    ("vendor"		.	"vendor.img")
    ("odm"		.	"odm.img")
    ("vbmeta"		.	"vbmeta.img")
    ("zimage"		.	"kernel")
    ("kernel"		.	"kernel")
    ("ramdisk"		.	"ramdisk.img"))
  "Fastboot flash targets associative list (NAME
. DEFAULT-FILE-NAME)."
  :group 'device-control)

(defsubst dctrl-fsb-default (target)
  (assoc-default target dctrl-fsb-targets))

(defun dctrl-fsb-read-file (label &optional file default-filename)
  (if (and file (file-exists-p file))
      file
    (let ((dir (when (and (boundp 'aosp-path) aosp-path
			  (boundp 'aosp-board-name) aosp-board-name)
		 (concat aosp-path "/out/target/product/"
			 aosp-board-name "/"))))
      (ido-read-file-name (format "%s file: " label)
			  dir default-filename t))))

(defun dctrl-fsb-read-target (&optional target)
  (or target
      (ido-completing-read "Target: " (mapcar 'car dctrl-fsb-targets))))

(defun dctrl-fsb-action-flash (&optional target file)
  (let* ((target (dctrl-fsb-read-target target))
	 (file (dctrl-fsb-read-file "Target" file (dctrl-fsb-default target))))
    (with-untramped-file file
      (dctrl-fsb-run "flash" target file))))

(defun dctrl-fsb-action-erase (&optional partition)
  (dctrl-fsb-run "erase" (dctrl-fsb-read-target partition)))

(defun dctrl-fsb-action-format (&optional partition)
  (dctrl-fsb-run "format" (dctrl-fsb-read-target partition)))

(defvar dctrl-fsb-oem-actions-list '("lock" "unlock" "verified"))

(defun dctrl-fsb-action-oem (&optional action)
  (let ((action (or action (ido-completing-read "Action: " dctrl-fsb-oem-actions-list nil t))))
    (dctrl-fsb-run "oem" action)))

(defun dctrl-fsb-action-flashing (&optional action)
  (dctrl-fsb-run "flashing" (ido-completing-read "Action: " '("lock" "unlock"))))

(defun dctrl-fsb-action-boot (&optional file)
  (let ((file (dctrl-fsb-read-file "Boot" file "boot.img")))
    (with-untramped-file file
      (dctrl-fsb-run "boot" file))))

(defun dctrl-fsb-action-flash-raw (&optional kernel ramdisk)
  (let ((kernel (dctrl-fsb-read-file "Kernel" kernel
				     (dctrl-fsb-default "kernel")))
	(ramdisk (dctrl-fsb-read-file "Ramdisk" ramdisk
				      (dctrl-fsb-default "ramdisk"))))
    (with-untramped-file kernel
      (with-untramped-file ramdisk
	(dctrl-fsb-run "flash:raw" kernel ramdisk)))))

(defun dctrl-fsb-action-continue ()
  (dctrl-fsb-run "continue"))

(defun dctrl-fsb-action-reboot ()
  (dctrl-fsb-run "reboot"))

(defun dctrl-fsb-action-reboot-bootloader ()
  (dctrl-fsb-run "reboot-bootloader"))

(defun dctrl-fsb-connected-p ()
  (let ((devices (dctrl-fsb-guess-device-names)))
    (if dctrl-automatic-mode
	devices
      (find dctrl-device-name devices :test 'string=))))

(defun dctrl-fsb-get-actions ()
  (dctrl-build-fun-list "dctrl-fsb-action-"
			(if (dctrl-fsb-connected-p) 'success 'error)))

(defconst fastboot-dev-line "^\\\([[:alnum:]]+\\\)[[:space:]]+fastboot$")

(defun dctrl-fsb-guess-device-names ()
  (mapcar (lambda (line)
	    (when (numberp (string-match fastboot-dev-line line))
	      (match-string 1 line)))
	  (split-string (shell-command-to-string
			 (concat fastboot-program " devices")) "\n" t)))

(dctrl-register-backend
 (make-dctrl-backend :name "fastboot"
		     :get-actions 'dctrl-fsb-get-actions
		     :guess-device-names 'dctrl-fsb-guess-device-names))

(provide 'dctrl-fastboot)
