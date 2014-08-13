(require 'device-control)

(defvar fastboot-exec "fastboot")

(defun dctrl-fastboot-run (&rest args)
  (dctrl-run-process
   (nconc (list fastboot-exec) (list "-p" dctrl-device-name) args)))

(defconst dctrl-fastboot-flash-alist '(("boot"		.	"boot.img")
				       ("fastboot"	.	"droidboot.img")
				       ("recovery"	.	"recovery.img")
				       ("system"	.	"system.img")))

(defun dctrl-fastboot-action-flash (&optional type file)
  (let* ((type (or type (ido-completing-read "Type of flash: " (mapcar 'car dctrl-fastboot-flash-alist) nil t)))
	 (file file)
	 tramp-cmd ctrlhost-filename)
    (unless (and file (file-exists-p file))
      (setq file (ido-read-file-name "Fastboot file: " nil
				     (assoc-default type ctrl-fastboot-flash-alist))))
  
    (multiple-value-setq (tramp-cmd ctrlhost-filename)
      (dctrl-untramp-file file))
    (append tramp-cmd
	    (dctrl-fastboot-run "flash" type (expand-file-name ctrlhost-filename)))))

(defun dctrl-fastboot-action-boot (&optional file)
  (let* ((file file)
	 tramp-cmd ctrlhost-filename)
    (unless (and file (file-exists-p file))
      (setq file (ido-read-file-name "Kernel file: " nil nil t)))
    (multiple-value-setq (tramp-cmd ctrlhost-filename)
      (dctrl-untramp-file file))
    (append tramp-cmd
	    (dctrl-fastboot-run "boot" (expand-file-name ctrlhost-filename)))))

(defun dctrl-fastboot-action-dnx ()
  (dctrl-fastboot-run "oem" "reboot" "dnx"))

(defun dctrl-fastboot-action-fastboot-continue ()
  (dctrl-fastboot-run "continue"))

(defun dctrl-fastboot-action-dnx ()
  (dctrl-fastboot-run "oem" "reboot" "dnx"))

(defun dctrl-fastboot-get-actions ()
  (dctrl-build-fun-list "dctrl-fastboot-action-"))

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

(provide 'device-control-fastboot)
