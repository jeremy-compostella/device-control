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
    (dctrl-flashrom-run (expand-file-name ctrlhost-filename))))

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

(provide 'device-control-flashrom)
