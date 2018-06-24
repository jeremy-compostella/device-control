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

(defvar fastboot-exec "fastboot")

(defun dctrl-fastboot-run (&rest args)
  (dctrl-run-process
   (nconc (list fastboot-exec) (if dctrl-automatic-mode
				   '()
				 (list "-p" dctrl-device-name))
	  args)))

(defvar dctrl-fastboot-flash-alist '(("boot"		.	"boot.img")
				     ("bootloader"	.	"bootloader.img")
				     ("recovery"	.	"recovery.img")
				     ("system"		.	"system.img")
				     ("vendor"		.	"vendor.img")
				     ("odm"		.	"odm.img")
				     ("vbmeta"		.	"vbmeta.img")
				     ("zimage"		.	"kernel")
				     ("kernel"		.	"kernel")
				     ("ramdisk"		.	"ramdisk.img")))

(defun dctrl-fastboot-aosp-out-dir ()
  (when (and aosp-path aosp-board-name)
    (concat aosp-path "/out/target/product/" aosp-board-name "/")))

(defun dctrl-fastboot-action-flash (&optional type file)
  (let* ((type (or type (ido-completing-read "Type of flash: " (mapcar 'car dctrl-fastboot-flash-alist) nil t)))
	 (file file)
	 tramp-cmd ctrlhost-filename)
    (unless (and file (file-exists-p file))
      (setq file (ido-read-file-name "File to flash: " (dctrl-fastboot-aosp-out-dir)
				     (assoc-default type dctrl-fastboot-flash-alist) t)))
    (multiple-value-setq (tramp-cmd ctrlhost-filename)
      (dctrl-untramp-file file))
    (append tramp-cmd
	    (dctrl-fastboot-run "flash" type (expand-file-name ctrlhost-filename)))))

(defvar dctrl-fastboot-partition-list '("system" "cache" "data" "misc" "recovery" "boot"))

(defun dctrl-fastboot-action-erase (&optional partition)
  (let ((partition (or partition (ido-completing-read "Partition name: " dctrl-fastboot-partition-list nil t))))
    (dctrl-fastboot-run "erase" partition)))

(defun dctrl-fastboot-action-format (&optional partition)
  (let ((partition (or partition (ido-completing-read "Partition name: " dctrl-fastboot-partition-list nil t))))
    (dctrl-fastboot-run "format" partition)))

(defvar dctrl-fastboot-oem-actions-list '("lock" "unlock" "verified"))

(defun dctrl-fastboot-action-oem (&optional action)
  (let ((action (or action (ido-completing-read "Action: " dctrl-fastboot-oem-actions-list nil t))))
    (dctrl-fastboot-run "oem" action)))

(defvar dctrl-fastboot-flashing-actions-list '("lock" "unlock"))

(defun dctrl-fastboot-action-flashing (&optional action)
  (let ((action (or action (ido-completing-read "Action: " dctrl-fastboot-flashing-actions-list nil t))))
    (dctrl-fastboot-run "flashing" action)))


(defun dctrl-fastboot-action-boot (&optional file)
  (let* ((file file)
	 tramp-cmd ctrlhost-filename)
    (unless (and file (file-exists-p file))
      (setq file (ido-read-file-name "Boot image: " (dctrl-fastboot-aosp-out-dir) "boot.img" t)))
    (multiple-value-setq (tramp-cmd ctrlhost-filename)
      (dctrl-untramp-file file))
    (append tramp-cmd
	    (dctrl-fastboot-run "boot" (expand-file-name ctrlhost-filename)))))

(defun dctrl-fastboot-action-flash-raw (&optional kernel ramdisk)
  (let ((dir (dctrl-fastboot-aosp-out-dir))
	(kernel (or kernel (ido-read-file-name "Kernel file: " (dctrl-fastboot-aosp-out-dir) "kernel" t)))
	(ramdisk (or ramdisk (ido-read-file-name "Ramdisk file: " (dctrl-fastboot-aosp-out-dir) "ramdisk.img" t)))
	tramp-cmd1 tramp-cmd2 ctrlhost-kernel-filename ctrlhost-ramdisk-filename)
    (multiple-value-setq (tramp-cmd1 ctrlhost-kernel-filename)
      (dctrl-untramp-file kernel))
    (multiple-value-setq (tramp-cmd2 ctrlhost-ramdisk-filename)
      (dctrl-untramp-file ramdisk))
    (append tramp-cmd1 tramp-cmd2
	    (dctrl-fastboot-run "flash:raw"
				(expand-file-name ctrlhost-kernel-filename)
				(expand-file-name ctrlhost-ramdisk-filename)))))

(defun dctrl-fastboot-action-continue ()
  (dctrl-fastboot-run "continue"))

(defun dctrl-fastboot-action-reboot ()
  (dctrl-fastboot-run "reboot"))

(defun dctrl-fastboot-action-reboot-bootloader ()
  (dctrl-fastboot-run "reboot-bootloader"))

(defun dctrl-fastboot-connected-p ()
  (let ((devices (dctrl-fastboot-guess-device-names)))
    (if dctrl-automatic-mode
	devices
      (find dctrl-device-name devices :test 'string=))))

(defun dctrl-fastboot-get-actions ()
  (dctrl-build-fun-list "dctrl-fastboot-action-"
			(if (dctrl-fastboot-connected-p) 'success 'error)))

(defconst fastboot-dev-line "^\\\([[:alnum:]]+\\\)[[:space:]]+fastboot$")

(defun dctrl-fastboot-guess-device-names ()
  (mapcar (lambda (line)
	    (when (numberp (string-match fastboot-dev-line line))
	      (match-string 1 line)))
	  (split-string (shell-command-to-string
			 (concat fastboot-exec " devices")) "\n" t)))

(dctrl-register-backend
 (make-dctrl-backend :name "fastboot"
		     :get-actions 'dctrl-fastboot-get-actions
		     :guess-device-names 'dctrl-fastboot-guess-device-names))

(provide 'dctrl-fastboot)
