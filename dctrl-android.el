;;; dctrl-android.el --- Device control Android meta-backend

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

;; This package is a `device-control' meta-backend aggregating
;; `dctrl-adb' abd `dctrl-fastboot' backends.


(require 'device-control)
(require 'dctrl-adb)
(require 'dctrl-fastboot)

(defun dctrl-android-get-actions ()
  (dctrl-agregate-fun-list (dctrl-build-fun-list "dctrl-adb-action-")
			   (dctrl-build-fun-list "dctrl-fastboot-action-")))

(defun dctrl-android-guess-device-names ()
  (nconc (dctrl-adb-guess-device-names)
	 (dctrl-fastboot-guess-device-names)))

(defun dctrl-android-show-online-devices ()
  (interactive)
  (let ((adb-devices (dctrl-adb-guess-device-names))
	(fastboot-devices (dctrl-fastboot-guess-device-names)))
    (if (or adb-devices fastboot-devices)
	(message (mapconcat 'identity
			    (append (mapcar (curry 'concat "adb:") adb-devices)
				    (mapcar (curry 'concat "fastboot:") fastboot-devices))
			    ", "))
      (message "No device available"))))

(dctrl-register-backend
 (make-dctrl-backend :name "android"
		     :get-actions 'dctrl-android-get-actions
		     :guess-device-names 'dctrl-android-guess-device-names))

(provide 'dctrl-android)
