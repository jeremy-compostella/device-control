(require 'device-control)

(defvar dctrl-relay-bootloader-combo '(vol-down vol-up))
(defvar dctrl-relay-device-prefix "/dev/serial/by-id/")
(defvar dctrl-relay-default-devices '("usb-Devantech_Ltd._USB-RLY08*"
				      "usb-FTDI_FT232R_USB_UART_A1009ZG8-*"))

(defvar dctrl-relay-map
  '((power	.	( ?i . ?s))
    (plug	.	( ?t . ?j))
    (vol-down	.	( ?l . ?v))
    (vol-up	.	( ?k . ?u))))

(defvar dctrl-relay-status-default
  (mapcar (lambda (x) (cons (car x) nil)) dctrl-relay-map))

(defvar-local dctrl-relay-device nil)
(defvar dctrl-relay-default-status (mapcar (lambda (x) (cons (car x) nil)) dctrl-relay-map))
(defvar-local dctrl-relay-status '())

(defun dctrl-relay-run (&rest args)
  (dctrl-run-process
   (nconc (list "sh" "-c"
		(mapconcat 'identity
			   (append (list "echo") args (list ">" dctrl-relay-device))
			   " ")))))

(defun dctrl-relay-send-command (cmd pushp)
  (let ((keys (assoc-default cmd dctrl-relay-map)))
    (setcdr (assoc cmd dctrl-relay-status) pushp)
    (dctrl-relay-run (char-to-string (if pushp (car keys) (cdr keys))))))

(defun dctrl-relay-toggle-command (cmd)
  (unless dctrl-relay-status
    (setq dctrl-relay-status dctrl-relay-default-status))
  (let ((keys (assoc-default cmd dctrl-relay-map))
	(new-status (not (assoc-default cmd dctrl-relay-status))))
    (setcdr (assoc cmd dctrl-relay-status) new-status)
    (dctrl-relay-send-command cmd new-status)))

(defun dctrl-relay-action-force-shutdown ()
  (nconc (dctrl-relay-send-command 'power t)
	 (dctrl-action-wait 11)
	 (dctrl-relay-toggle-command 'power)))

(defun dctrl-relay-action-power-on ()
  (nconc (dctrl-relay-send-command 'power t)
	 (dctrl-action-wait 3)
	 (dctrl-relay-toggle-command 'power)))

(defun dctrl-relay-action-force-reboot ()
  (nconc (dctrl-relay-action-force-shutdown)
	 (dctrl-relay-action-power-on)))

(defun dctrl-relay-action-force-bootloader ()
  (nconc (dctrl-relay-action-force-shutdown)
	 (dolist (cmd dctrl-relay-bootloader-combo)
	   (dctrl-relay-send-command cmd t))
	 (dctrl-relay-action-power-on)
	 (dctrl-action-wait 10)
	 (dolist (cmd dctrl-relay-bootloader-combo)
	   (dctrl-relay-toggle-command cmd))))

(defun dctrl-relay-select-device ()
  (let* ((prefix (concat (dctrl-get-tramp-prefix) dctrl-relay-device-prefix))
	 (files (mapcar 'file-expand-wildcards
			(mapcar (curry 'concat prefix)
				dctrl-relay-default-devices)))
	 (files (apply 'append (delq nil files))))
    (when files
      (let ((dev (ido-completing-read "Relay device: "
				      (append (mapcar 'file-name-nondirectory files) '("none"))
				      nil t)))
	(cond ((string= dev "none") (progn (setq dctrl-relay-device 'none)
					   nil))
	      ((setq dctrl-relay-device (concat dctrl-relay-device-prefix dev))))))))

(defun dctrl-relay-connected-p ()
  (cond ((eq dctrl-relay-device 'none) nil)
	(dctrl-relay-device dctrl-relay-device)
	(t (dctrl-relay-select-device))))

(defun dctrl-relay-get-actions ()
  (when (dctrl-relay-connected-p)
    (dctrl-build-fun-list "dctrl-relay-action-" 'success)))

(defun dctrl-relay-init ()
  (setq dctrl-relay-status dctrl-relay-default-status))

(dctrl-register-backend
 (make-dctrl-backend :name "relay"
		     :create 'dctrl-relay-init
		     :get-actions 'dctrl-fastboot-get-actions))

(provide 'device-control-relay)
