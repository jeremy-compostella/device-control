(require 'device-control)

(defvar adb-exec "adb")

(defvar adb-push-file-history '())

(defun dctrl-adb-run (&rest args)
  (dctrl-run-process
   (nconc (list adb-exec) (if dctrl-automatic-mode
			      '()
			    (list "-s" dctrl-device-name)) args)))

(defun dctrl-adb-action-reboot ()
  (dctrl-adb-run "reboot"))

(defvar dctrl-adb-reboot-target '("" "bootloader" "recovery"))

(defun dctrl-adb-action-reboot (&optional target)
  (let ((target (or target (ido-completing-read "Target: " dctrl-adb-reboot-target nil t))))
    (dctrl-adb-run "reboot" target)))

(defun dctrl-adb-action-shutdown ()
  (dctrl-adb-run "shell" "am" "start" "-a"
		 "android.intent.action.ACTION_REQUEST_SHUTDOWN"))

(defun dctrl-adb-action-push-file (&optional filename dst-filename)
  (let* ((ido-file-history 'adb-push-file-history)
	 (src (or filename (ido-read-file-name "Source: " )))
	 (dst (or dst-filename (read-string "Target: " nil)))
	 tramp-cmd ctrlhost-filename)
    (multiple-value-setq (tramp-cmd ctrlhost-filename)
      (dctrl-untramp-file src))
    (append tramp-cmd (dctrl-adb-run "push" ctrlhost-filename dst))))

(defun dctrl-adb-aosp-out-dir ()
  (when (and aosp-path aosp-board-name)
    (concat aosp-path "/out/target/product/" aosp-board-name "/")))

(defun dctrl-adb-action-ota (&optional file)
  (let ((file file)
	tramp-cmd ctrlhost-filename)
    (unless (and file (file-exists-p file))
      (setq file (ido-read-file-name "File to flash: " (dctrl-adb-aosp-out-dir))))
    (multiple-value-setq (tramp-cmd ctrlhost-filename)
      (dctrl-untramp-file file))
    (append tramp-cmd
	    (dctrl-adb-run "root")
	    (dctrl-action-wait 2)
	    (dctrl-adb-run "push" (expand-file-name ctrlhost-filename) "/data/local/tmp/update.zip")
	    (dctrl-adb-run "shell" "mkdir" "-p" "/cache/recovery")
	    (dctrl-adb-run "shell" "echo '--update_package=/data/local/tmp/update.zip' > /cache/recovery/command")
	    (dctrl-adb-run "shell" "start" "pre-recovery"))))

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

(provide 'device-control-adb)
