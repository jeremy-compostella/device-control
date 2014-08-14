(require 'ido)
(require 'device-control)
(require 'device-control-adb)
(require 'device-control-fastboot)

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
      (message "No devices available"))))

(dctrl-register-backend
 (make-dctrl-backend :name "android"
		     :get-actions 'dctrl-android-get-actions
		     :guess-device-names 'dctrl-android-guess-device-names))

(provide 'device-control-android)
