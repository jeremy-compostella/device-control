(require 'device-control)

(defvar adb-exec "adb")

(defun dctrl-adb-run (&rest args)
  (dctrl-run-process
   (nconc (list adb-exec) (list "-s" dctrl-device-name) args)))

(defun dctrl-adb-action-reboot ()
  (dctrl-adb-run "reboot"))

(defun dctrl-adb-action-efi-shell ()
  (dctrl-adb-run "shell" "uefivar" "-g" "8be4df61-93ca-11d2-aa0d-00e098032b8c" "-n" "BootNext" "-t" "int16" "-s" "1"))

(defun dctrl-adb-action-dnx ()
  (dctrl-adb-run "reboot" "dnx"))

(defun dctrl-adb-action-recovery ()
  (dctrl-adb-run "reboot" "recovery"))

(defun dctrl-adb-action-bootloader ()
  (dctrl-adb-run "reboot" "bootloader"))

(defun dctrl-adb-action-shutdown ()
  (dctrl-adb-run "shell" "am" "start" "-a"
		 "android.intent.action.ACTION_REQUEST_SHUTDOWN"))

(defun dctrl-adb-action-push-file (&optional filename dst-filename)
  (let* ((src (or filename (ido-read-file-name "Source: ")))
	 (dst (or dst-filename (read-string "Target: " nil)))
	 tramp-cmd ctrlhost-filename)
    (multiple-value-setq (tramp-cmd ctrlhost-filename)
      (dctrl-untramp-file src))
    (append tramp-cmd (dctrl-adb-run "push" ctrlhost-filename dst))))

(defun dctrl-adb-get-actions ()
  (dctrl-build-fun-list "dctrl-adb-action-"))

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

(provide 'device-control-adb)
