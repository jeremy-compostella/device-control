;;; device-control.el --- Device control

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

;; This package provides control of a device such as an Android
;; device, would it be locally connected or remotely connected through
;; tramp.  The "control" relies on a backend, which provides commands
;; available on the device.

(require 'cl)
(require 'font-lock)
(require 'ido)
(require 'notifications)
(require 'tramp)

(defgroup device-control nil
  "Provide control of a locally or remotely connected device."
  :group 'tools
  :group 'processes)

(defcustom dctrl-icon nil
  "Path to graphic file to be used as `device-control' module
notification icon."
  :group 'device-control)

(defcustom dctrl-time-format "[%Y/%m/%d %H:%M:%S]"
  "Time format use to prefix events in the device control
buffers."
  :group 'device-control)

(defcustom dctrl-buf-fmt "*dctrl:%s-%s*"
  "Device Control buffer name format.  The first string is the
backend name and the second string is the device name."
  :group 'device-control)

(defvar dctrl-backends '()
  "List of regitered backends through `dctrl-register-backend'.")

(defconst dctrl-empty-fifo (cons 'dctrl-actions nil))

(defvar-local dctrl-actions (copy-list dctrl-empty-fifo)
  "FIFO of actions to perform on a device.
Normally this list is fully populated with `dctrl-run-process'
calls.

The format of this FIFO is a list of (function), where : -
function is the function to call to perform the action
ex: (lambda() (process-file \"ls\")) The function should return
the state the action FIFO should be put to after the action.
")
(defvar-local dctrl-backend nil
  "The backend handling the controlled device.")
(defvar-local dctrl-state 'stopped
  "The action FIFO state for the controlled device.
Might be 'stopped', 'running' or 'sleep'.")
(defvar-local dctrl-device nil
  "The device name provided at `device-control-start' time.")
(defvar-local dctrl-automatic-mode nil
  "Don't use any specific device, use the one which is currently
connected indifferently.")
(defvar-local dctrl-prev-notif-id nil
  "Previous notification ID.")

(defvar device-control-action-history '()
  "History of actions.")
(defvar dctrl-host-history '()
  "History of host.")

;; Backend interface
(defstruct dctrl-backend
  name				; Unique name of the backend
  (create 'ignore)		; Hook just before creating the control buffer
  (start 'ignore)		; Hook just before starting a FIFO execution
  (pause 'ignore)		; Hook just before pausing a FIFO execution
  (resume 'ignore)		; Hook just before resuming a FIFO execution
  (stop 'ignore)		; Hook just before stopping the FIFO execution
  (cancel 'ignore)		; Hook just before cancelling the current execution
  (kill 'ignore)		; Hook just before killing the current action
  (guess-device-names 'ignore)	; Backend might hint device names (completion)
  get-actions)			; Provides backend available actions
				; alist (NAME . fct_action)

(defun dctrl-register-backend (backend)
  "Register a backend to the device control available backends.
This is the function a device control backend must call to make
its functions available to device control."
  (add-to-list 'dctrl-backends backend nil
	       (lambda (x y) (string= (dctrl-backend-name x)
				      (dctrl-backend-name y)))))

;; Internals
(defun dctrl-get-backend-by-name (backend-name)
  (find backend-name dctrl-backends :test 'string= :key 'dctrl-backend-name))

(defun dctrl-append (&rest args)
  (let ((inhibit-read-only t)
	(return-position))
    (unless (= (point-max) (point))
      (setq return-position (point))
      (goto-char (point-max)))
    (apply 'insert args)
    (if return-position
	(goto-char return-position)
      (when (get-buffer-window (current-buffer))
	(set-window-point (get-buffer-window (current-buffer)) (point-max))))))

(defsubst dctrl-current-time-string ()
  (propertize (format-time-string dctrl-time-format (current-time))
	      'face 'font-lock-comment-face))

(defun dctrl-msg (msg &optional face)
  (dctrl-append (dctrl-current-time-string) " "
		(propertize msg 'face face) "\n"))

(defun dctrl-headline-msg (msg)
  (when dctrl-prev-notif-id
    (notifications-close-notification dctrl-prev-notif-id))
  (when (and (featurep 'dbusbind) (notifications-get-server-information))
    (setq dctrl-prev-notif-id
          (apply 'notifications-notify
                 :app-name "DeviceControl"
                 :title "Device control"
                 :body msg
                 (when dctrl-icon
               (list :app-icon dctrl-icon)))))
  (dctrl-msg msg 'success))

(defun dctrl-error (msg)
  (dctrl-msg msg 'error)
  (error msg))

(defun dctrl-warn (msg)
  (dctrl-msg msg 'warning)
  (message "device-control: %s" msg))

;; Actions FIFO management
(defun dctrl-continue ()
  (funcall (dctrl-backend-resume dctrl-backend))
  (if (not (cdr dctrl-actions))
      (dctrl-stop 'success)
    (let ((action (cadr dctrl-actions)))
      (setcdr dctrl-actions (cddr dctrl-actions))
      (let ((type (funcall action)))
	(cond ((eq type 'continue) (dctrl-continue))
	      ((eq type 'sleep))
	      (t (dctrl-stop 'failure type)))))))

(defun dctrl-start ()
  (unless (eq dctrl-state 'running)
    (dctrl-headline-msg "Actions FIFO execution started.")
    (funcall (dctrl-backend-start dctrl-backend))
    (setq dctrl-state 'running)
    (dctrl-continue)))

(defun dctrl-stop (type &optional msg)
  (funcall (dctrl-backend-stop dctrl-backend))
  (setq dctrl-state 'stopped)
  (cond ((eq type 'failure)
	 (dctrl-error (format "Last action failed: %s." (or msg "unknown reason"))))
	((eq type 'success)
	 (dctrl-headline-msg "Actions FIFO execution terminated."))
	((eq type 'clear)
	 (setq dctrl-actions (copy-list dctrl-empty-fifo)))))

(defun dctrl-buf-list (var-name var-value &optional bufs)
  (let ((my-bufs (or bufs (dctrl-buffers))))
    (delete-if-not (curry 'eq var-value) my-bufs
		    :key (curry 'buffer-local-value var-name))))

(defun dctrl-kill-current (&optional device-name)
  "Kill the current action."
  (interactive)
  (funcall (dctrl-backend-kill dctrl-backend))
  (when (get-buffer-process (current-buffer))
    (kill-process)))

(defun dctrl-create-buffer (device-name backend-name host)
  (let ((buf (get-buffer-create (format dctrl-buf-fmt backend-name device-name)))
	(backend (dctrl-get-backend-by-name backend-name)))
    (with-current-buffer buf
      (device-control-mode)
      (setq dctrl-state 'stopped
	    dctrl-backend backend
	    dctrl-actions (copy-list dctrl-empty-fifo))
      (if (string= "localhost" host)
	  (setq default-directory "~")
	(setq default-directory (format "/ssh:%s:" host)))
      (if (string= device-name "automatic")
	  (progn (setq dctrl-automatic-mode t)
		 (setq dctrl-device (read-string "Give a name to your device: "))
		 (rename-buffer (format dctrl-buf-fmt backend-name dctrl-device)))
	  (setq dctrl-device device-name))
      (funcall (dctrl-backend-create dctrl-backend)))
    buf))

(defsubst dctrl-buffers ()
  (delete-if-not (curry 'eq 'device-control-mode)
		 (buffer-list)
		 :key (curry 'buffer-local-value 'major-mode)))

(defun dctrl-get-buffer (device-name)
  (find device-name (dctrl-buffers)
	:test 'string=
	:key (curry 'buffer-local-value 'dctrl-device)))

(defun dctrl-get-list-devices (&optional state backend-name)
  (let ((state-bufs (if state (dctrl-buf-list 'dctrl-state state) (dctrl-buffers))))
    (mapcar (curry 'buffer-local-value 'dctrl-device)
	    (if backend-name
		(dctrl-buf-list 'dctrl-backend backend-name state-bufs)
	      state-bufs))))

(defvar dctrl-last-used-device nil)	;TODO: make a list of used devices

(defsubst dctrl-same-backend-same-host (buf1 buf2)
  (and (string= (dctrl-backend-name (buffer-local-value 'dctrl-backend buf1))
		(dctrl-backend-name (buffer-local-value 'dctrl-backend buf2)))
       (string= (dctrl-get-buffer-host (buffer-local-value 'default-directory buf1))
		(dctrl-get-buffer-host (buffer-local-value 'default-directory buf2)))))

(defun dctrl-online-devices ()
  (let ((bufs (delete-duplicates (dctrl-buffers) :test 'dctrl-same-backend-same-host)))
    (apply 'append
	   (mapcar (lambda (x) (with-current-buffer x
				 (funcall (dctrl-backend-guess-device-names
					   (buffer-local-value 'dctrl-backend x)))))
		   bufs))))

(defun dctrl-colorize-devices (devices)
  (let ((onlines (dctrl-online-devices)))
    (mapcar (lambda (x) (propertize x 'face
				    (if (find x onlines :test 'string=)
					'success
				      'error)))
	    devices)))

(defun dctrl-device-score (device)
  (+ (if (eq 'success (get-text-property 0 'face device)) 1 0)
     (if (string= dctrl-last-used-device device) 2 0)))

(defun dctrl-smart-order (devices)
  (let ((devices (dctrl-colorize-devices devices)))
    (sort devices (lambda (x y) (> (dctrl-device-score x)
				   (dctrl-device-score y))))))

(defun dctrl-read-device (&optional state backend-name)
  (interactive)
  (let ((devices-list (dctrl-get-list-devices state backend-name)))
    (cond ((eq major-mode 'device-control-mode) dctrl-device)
	  ((= 0 (length devices-list)) (device-control-start))
	  ((= 1 (length devices-list)) (car devices-list))
	  ((ido-completing-read "Device: " (dctrl-smart-order devices-list))))))

(defun dctrl-get-tramp-prefix (&optional dir)
  (let ((dir (or dir default-directory)))
    (or (and (tramp-tramp-file-p dir)
	     (with-parsed-tramp-file-name dir info
	       (concat "/" info-method ":"
		       (if info-user (concat info-user "@") "")
		       info-host ":")))
	"")))

(defun dctrl-get-buffer-host (&optional dir)
  (let ((dir (or dir default-directory)))
    (or (and (tramp-tramp-file-p dir)
	     (with-parsed-tramp-file-name dir info
	       info-host))
	"localhost")))

(defun dctrl-read-backend (host)
  (ido-completing-read (format "Device control backend (on %s): " host)
		       (mapcar 'dctrl-backend-name dctrl-backends)))

(defun dctrl-read-new-device (host backend)
  (ido-completing-read (format "Device name (on %s): " host)
		       (delq nil (nconc '("automatic")
					(funcall (dctrl-backend-guess-device-names
						  (dctrl-get-backend-by-name backend)))))))
;; End user functions
(defun device-control-start (&optional host backend device)
  "Create a device controller.  A device controller is a buffer
associated to a HOST, a BACKEND and a DEVICE."
  (interactive)
  (let* ((host (or host (read-string "Host: " (dctrl-get-buffer-host)
				     'dctrl-host-history)))
	 (backend (or backend (dctrl-read-backend host)))
	 (device (or device (dctrl-read-new-device host backend))))
    (when device
      (with-current-buffer (dctrl-create-buffer device backend host)
	dctrl-device))))

(defun device-control (device-name)
  "Enqueue a new action in the actions fifo for a device.
Start the FIFO execution if it is not started yet."
  (interactive (list (dctrl-read-device)))
  (setq dctrl-last-used-device device-name)
  (with-current-buffer (dctrl-get-buffer device-name)
    (let* ((actions (funcall (dctrl-backend-get-actions dctrl-backend)))
	   (action (ido-completing-read (format "Action (%s on %s): "
						device-name
						(dctrl-get-buffer-host))
					(mapcar 'car actions) nil t nil
					'device-control-action-history)))
      (nconc dctrl-actions (funcall (assoc-default action actions)))
      (dctrl-start))))

(defun dctrl-cancel (device-name)
  "Cancel the current action and pause the fifo execution."
  (interactive (list (dctrl-read-device 'running)))
  (with-current-buffer (dctrl-get-buffer device-name)
    (unless (eq dctrl-state 'running)
      (error "device-control: selected control buffer is not in running state."))
    (dctrl-kill-current)
    (funcall (dctrl-backend-cancel dctrl-backend))
    (setf dctrl-state 'paused)
    (dctrl-warn "Actions FIFO execution canceled.")))

(defun dctrl-cancel-and-clear (&optional device-name)
  "Cancel the current action and pause the fifo execution."
  (interactive (list (dctrl-read-device 'running)))
  (with-current-buffer (dctrl-get-buffer device-name)
    (dctrl-kill-current)
    (dctrl-stop 'clear)
    (dctrl-warn "Actions FIFO execution canceled and FIFO cleared.")))

(defun dctrl-pause (&optional device-name)
  "Let finish the current and pause the fifo execution."
  (interactive (list (dctrl-read-device 'running)))
  (with-current-buffer (dctrl-get-buffer device-name)
    (unless (eq dctrl-state 'running)
      (error "device-control: selected control buffer is not in running state."))
    (funcall (dctrl-backend-pause dctrl-backend))
    (setf dctrl-state 'paused)
    (dctrl-warn "Actions FIFO execution paused.")))

(defun dctrl-resume (&optional device-name)
  "Resume the fifo execution. See `dctrl-pause' and `dctrl-cancel'"
  (interactive (list (dctrl-read-device 'paused)))
  (with-current-buffer (dctrl-get-buffer device-name)
    (unless (eq dctrl-state 'paused)
      (error "device-control: selected control buffer is not in paused state."))
    (funcall (dctrl-backend-resume dctrl-backend))
    (dctrl-start)))

(defun dctrl-kill-device (device-name)
  (interactive (list (dctrl-read-device)))
  (kill-buffer (dctrl-get-buffer device-name)))

;; Mode
(defvar device-control-mode-map nil
  "Keymap for device-control major mode.")
(unless device-control-mode-map
  (setq device-control-mode-map (make-sparse-keymap))
  (define-key device-control-mode-map "d" 'device-control)
  (define-key device-control-mode-map "c" 'dctrl-cancel)
  (define-key device-control-mode-map "C" 'dctrl-cancel-and-clear)
  (define-key device-control-mode-map "p" 'dctrl-pause)
  (define-key device-control-mode-map "r" 'dctrl-resume)
  (define-key device-control-mode-map "q" 'quit-window))

(define-derived-mode device-control-mode fundamental-mode
  "device-control"
  "device-control major mode.
Special commands:
\\{device-control-mode-map}"
  (toggle-read-only t))

;; Tools
(defun dctrl-build-fun-list (prefix &optional face)
  "Build the alist (FUNCTION_NAME . FUNCTION) of all functions
defined matching the given prefix. All get-actions() backend
functions should use this."
  (let ((fun-list (list '("a" . 'b))))
    (mapatoms (lambda (x)
		(when (and (string-prefix-p prefix (symbol-name x))
			   (symbol-function x))
		  (let ((name (propertize (substring (symbol-name x)
						     (length prefix))
					  'face face)))
		    (nconc fun-list (list (cons name x)))))))
    (sort (cdr fun-list) (lambda (x y) (string< (car x) (car y))))))

(defun dctrl-include-backend-in-name (cell)
  (let ((symname (symbol-name (cdr cell))))
    (string-match "dctrl-\\\([[:alnum:]]+\\\)-action" symname)
    (setcar cell (propertize (concat (match-string 1 symname) ":" (car cell))
			     'face (get-text-property 0 'face (car cell))))))

(defun dctrl-agregate-fun-list (&rest lists)
  (let ((all (sort (apply 'append lists)
		   (lambda (x y) (string< (car x) (car y))))))
    (let ((cur all))
      (while (cdr cur)
	(when (string= (caar cur) (caadr cur))
	  (let ((action (caar cur)))
	    (dctrl-include-backend-in-name (car cur))
	    (dctrl-include-backend-in-name (cadr cur))))
	(setq cur (cdr cur))))
    (cl-sort all (lambda (x y) (and (eq 'success (get-text-property 0 'face x))
				    (eq 'error (get-text-property 0 'face y))))
	     :key 'car)))

(defun dctrl-process-sentinel (p e)
  (with-current-buffer (process-buffer p)
    (if (string= e "finished\n")
	(when (eq dctrl-state 'running)
	  (dctrl-continue))
      (dctrl-stop 'failure))))

(defun dctrl-process-filter (p str)
  (with-current-buffer (process-buffer p)
    (dctrl-append (replace-regexp-in-string "\r" "\n" str))))

(defmacro dctrl-internal-run-process (args start-process-fun)
  `(lexical-let ((args args))
     (list (lambda ()
	     (dctrl-msg (mapconcat 'identity ,args " "))
	     (set-process-sentinel (apply ,start-process-fun "ctrl" (current-buffer) ,args)
				   'dctrl-process-sentinel)
	     (set-process-filter (get-buffer-process (current-buffer))
				 'dctrl-process-filter)
	     'sleep))))

(defun dctrl-run-process (args)
  (dctrl-internal-run-process args 'start-file-process))

(defun dctrl-run-local-process (args)
  (dctrl-internal-run-process args 'start-process))

(defun dctrl-action-wait (&optional seconds)
  (let ((seconds (or seconds (read-number "Seconds: "))))
    (dctrl-run-process (list "sleep" (format "%ds" seconds)))))

(defun dctrl-untramp-file (src-file)
  "Transfer the file onto the tramp host of the controller, and
return the remote filename.  If the tramp host controller *and*
src-file is localhost, leave the file where it is.

In all cases, returns a list of :
 - the list representing the command to run
 - the filename part on the device controller host."
  (let* ((src-host (and (tramp-tramp-file-p src-file)
			(with-parsed-tramp-file-name src-file s s-host)))
	 (src-filename (or
			(and src-host (with-parsed-tramp-file-name src-file s s-localname))
			src-file))
	 (src-user (if (tramp-tramp-file-p src-file)
		       (with-parsed-tramp-file-name src-file s
			 (if s-user (format "%s@" s-user) ""))
		     "" ))
	 (dst-host (and (tramp-tramp-file-p default-directory)
			(with-parsed-tramp-file-name default-directory d d-host)))
	 (dst-filename (concat (or (getenv "TMPDIR") "/tmp") "/"
			       (file-name-nondirectory src-file)))
	 (dst-user (if (tramp-tramp-file-p default-directory)
		       (with-parsed-tramp-file-name default-directory d
			 (if d-user (format "%s@" d-user) ""))
		     "" ))
	 xfer cmd final-filename)
    (cond
     ; Transfering remote src-host:file to localhost:/tmp
     ((and src-host (not dst-host))
      (setq xfer 'dctrl-run-local-process
	    cmd (list "scp" (format "%s%s:%s" src-user src-host src-filename)
		      dst-filename)
	    final-filename dst-filename))
     ; Keeping the source file on localhost as all is on localhost, or all
     ; on remote host if all is remote
     ((or (not dst-host) (string= src-host dst-host))
      (setq final-filename src-filename))
     ; Transfering local file to remote dst-host:/tmp
     ((not src-host)
      (setq xfer 'dctrl-run-local-process
	    cmd (list "scp" src-filename
		      (format "%s%s:%s" dst-user dst-host dst-filename))
	    final-filename dst-filename))
     ; Transfering remote src-host:file to dst-host:/tmp
     (t
      (setq xfer 'dctrl-run-process
	    cmd (list "scp" (format "%s%s:%s" src-user src-host src-filename)
		      dst-filename)
	    final-filename dst-filename)))
    (if xfer
	(list (apply xfer (list cmd))
	      (expand-file-name final-filename))
      (list nil (expand-file-name final-filename)))))

(defmacro with-untramped-file (file &rest body)
  "This macro calls `dctrl-untramp-file' to generate the
necessary commands to transfer FILE onto the host of the
controller.  It appends these commands with the commands
generated by BODY.  The FILE variable is set with the file path
on the host of the controller.  It allows simple construction
like the following:

  (with-untramped-file file
    (dctrl-run-process (list \"fastboot\" \"flash\" \"boot\" file)))"
  (declare (indent 1))
  (let ((temp (make-symbol "--dctrl--var--")))
     `(progn (multiple-value-setq (,temp ,file)
	       (dctrl-untramp-file ,file))
	     (append ,temp ,@body))))

(defsubst curry (function &rest arguments)
  (lexical-let ((function function)
		(arguments arguments))
    (lambda (&rest more) (apply function (append arguments more)))))

(defsubst rcurry (function &rest arguments)
  (lexical-let ((function function)
		(arguments arguments))
    (lambda (&rest more) (apply function (append more arguments)))))

(provide 'device-control)
