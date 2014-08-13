(require 'ido)
(require 'device-control)
(require 'device-control-adb)
(require 'device-control-fastboot)

(defun dctrl-android-get-actions ()
  (dctrl-agregate-fun-list (dctrl-build-fun-list "dctrl-adb-action-")
			   (dctrl-build-fun-list "dctrl-fastboot-action-")))

(defun dctrl-intel-guess-device-names ()
  (nconc (dctrl-adb-guess-device-names)
	 (dctrl-fastboot-guess-device-names)))

(dctrl-register-backend
 (make-dctrl-backend :name "intel"
		     :get-actions 'dctrl-intel-get-actions
		     :guess-device-names 'dctrl-intel-guess-device-names))

(provide 'device-control-intel)
