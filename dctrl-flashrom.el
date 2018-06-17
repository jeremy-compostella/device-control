;;; dctrl-flashrom.el --- Device control for the flashrom
;;; utility

;; Copyright (C) 2014-2018 Free Software Foundation, Inc.

;; Author: Jeremy Compostella <jeremy.compostella@gmail.com>
;;         Robert Jarzmik <robert.jarzmik@free.fr>
;;         Sylvain Chouleur <sylvain.chouleur@gmail.com>

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

;; This package is a `device-control' backend based on the 'flashrom'
;; utility.

(require 'device-control)

(defvar flashrom-exec "flashrom")
(defvar flashrom-device-id ".*0483:dada.*")

(defun dctrl-flashrom-run (&rest args)
  (dctrl-run-process
   (nconc (list flashrom-exec) (list "-p" "dediprog:voltage=1.8V" "-w") args)))

(defun dctrl-flashrom-action-flash (&optional file)
  (let ((file (read-file-name "Flash file: " nil nil t))
	tramp-cmd ctrlhost-filename)
    (multiple-value-setq (tramp-cmd ctrlhost-filename)
      (dctrl-untramp-file file))
    (append tramp-cmd
	    (dctrl-flashrom-run (expand-file-name ctrlhost-filename)))))

(defun dctrl-flashrom-device-present-p ()
  (delq nil (mapcar (lambda (line)
		      (numberp (string-match flashrom-device-id line)))
		    (split-string (shell-command-to-string "lsusb") "\n" t))))

(defun dctrl-flashrom-get-actions ()
  (when (dctrl-flashrom-device-present-p)
    (dctrl-build-fun-list "dctrl-flashrom-action-" 'success)))

(dctrl-register-backend
 (make-dctrl-backend :name "flashrom"
		     :get-actions 'dctrl-flashrom-get-actions))

(provide 'dctrl-flashrom)
