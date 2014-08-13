;;; device-control.el --- Device control

;; Copyright (C) 2014 2014 Free Software Foundation, Inc.

;; Author: Jeremy Compostella <jeremy.compostella@gmail.com>
;;         Robert Jarzmik <robert.jarzmik@free.fr>
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

;; This package provide control a device such as an Android device, would it be
;; locally connected or remotely connected through tramp.  The "control" relies
;; on a backend, which provides commands available on the device.
;; Notes:
;; -----
;;
;; This package only works for Emacs 24 and higher.

(require 'ido)

(defconst dctrl-buf-prefix "*ctrl-")
(defconst dctrl-buf-fmt (concat dctrl-buf-prefix "%s*"))

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
(defvar-local dctrl-avail-actions nil
  "The available actions of a device.
This is an alist of (name . function), where :
  - name is the string of the action name (provided to completion)
  - function is the function called for this action")
(defvar-local dctrl-state 'stopped
  "The action FIFO state for the controlled device.
Might be 'stopped, 'running or 'sleep.")
(defvar-local dctrl-device-name nil
  "The device name provided at `device-control-start' time.")

;; Backend interface
;; To be implemented by each backend with dctrl-backend-register().
(defstruct dctrl-backend
  name				; Unique name of a backend type
  (create 'ignore)		; Hook just before creating the control buffer
  (start 'ignore)		; Hook just before starting a FIFO execution
  (pause 'ignore)		; Hook just before pausing a FIFO execution
  (resume 'ignore)		; Hook just before resuming a FIFO execution
  (stop 'ignore)		; Hook just before stopping the FIFO execution
  (cancel 'ignore)		; Hook just before cancelling the current execution
  (kill 'ignore)		; Hook just before killing the current action
  (guess-device-names 'ignore)	; Backend might hint device names (completion)
  get-actions)			; Provides backend available actions alist (NAME . fct_action)

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

(defun dctrl-append-msg (msg &optional face)
  (save-excursion-if-not-at-point-max (current-buffer)
    (goto-char (point-max))
    (insert (propertize (format-time-string "[%Y/%m/%d %H:%M:%S] " (current-time))
			'face (or face 'success)) msg "\n")))

(defun dctrl-msg (msg)
  (dctrl-append-msg msg))

(defun dctrl-headline-msg (msg)
  (dctrl-append-msg (propertize msg 'face 'success)))

(defun dctrl-error (msg)
  (dctrl-append-msg (propertize msg 'face 'error))
  (error msg))

(defun dctrl-warn (msg)
  (dctrl-append-msg (propertize msg 'face 'font-lock-warning-face))
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
	      (dctrl-stop 'failure type))))))

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

(defmacro dctrl-buf-list (var-name var-value &optional bufs)
  (declare (indent 1))
  `(let ((my-bufs (or bufs (dctrl-buffers))))
     (delete-if-not (curry 'eq ,var-value) my-bufs
		    :key (curry 'buffer-local-value ,var-name))))

(defun dctrl-buffer-lookup (state)
  (let ((bufs (dctrl-buf-list 'dctrl-state state)))
    (cond ((= (length bufs) 0) (error "No buffer found"))
	  ((= (length bufs) 1) (car bufs))
	  (get-buffer (ido-completing-read "Buffer: " (mapcar 'buffer-name bufs))))))

(defun dctrl-smart-buf-selection (device state)
  (or (and device (get-buffer (format dctrl-buf-fmt device)))
      (and (eq major-mode 'device-control-mode) (current-buffer))
      (dctrl-buffer-lookup state)))

(defun dctrl-kill-current (&optional device-name)
  "Kill the current action."
  (interactive)
  (funcall (dctrl-backend-kill dctrl-backend))
  (when (get-buffer-process (current-buffer))
    (kill-process)))

(defun dctrl-create-buffer(device-name backend-name)
  (let ((buf (get-buffer-create (format dctrl-buf-fmt device-name)))
	(backend (dctrl-get-backend-by-name backend-name)))
    (with-current-buffer buf
      (device-control-mode)
      (setq dctrl-state 'stopped
	    dctrl-backend backend
	    dctrl-device-name device-name
	    dctrl-avail-actions (funcall (dctrl-backend-get-actions backend))
	    dctrl-actions (copy-list dctrl-empty-fifo))
      (funcall (dctrl-backend-create dctrl-backend)))
    buf))

(defsubst dctrl-buffers ()
  (delete-if-not (curry 'eq 'device-control-mode)
		 (buffer-list)
		 :key (curry 'buffer-local-value 'major-mode)))

(defun dctrl-get-buffer (device-name)
  (find device-name (dctrl-buffers)
	:test 'string=
	:key (curry 'buffer-local-value 'dctrl-device-name)))

(defun dctrl-get-list-devices (&optional state backend-name)
  (let ((state-bufs (if state (dctrl-buf-list 'dctrl-state state) (dctrl-buffers))))
    (mapcar (curry 'buffer-local-value 'dctrl-device-name)
	    (if backend-name
		(dctrl-buf-list 'dctrl-backend backend-name state-bufs)
	      state-bufs))))

(defun dctrl-complete-device (&optional state backend-name)
  (interactive)
  (let ((devices-list (dctrl-get-list-devices state backend-name)))
    (cond ((eq major-mode 'device-control-mode) dctrl-device-name)
	  ((= 0 (length devices-list)) (device-control-start))
	  ((= 1 (length devices-list)) (car devices-list))
	  ((ido-completing-read "Device: " devices-list)))))

;; Interactives
(defun device-control-start (&optional backend-name device-name)
  "Create a device controller, requiring a backend type. The
backend should have been registered with device-control-register-backend."
  (interactive)
  (unless backend-name
    (setq backend-name
	  (ido-completing-read "Device control backend: "
			       (mapcar 'dctrl-backend-name dctrl-backends))))
  (unless device-name
    (setq device-name
	  (ido-completing-read "Device name: "
			   (funcall (dctrl-backend-guess-device-names
				     (dctrl-get-backend-by-name backend-name))))))
  (when device-name
    (dctrl-create-buffer device-name backend-name))
  device-name)

(defun device-control (device-name)
  "Enqueue a new action in the actions fifo for a device.
  Start the fifo execution if not running yet."
  (interactive (list (dctrl-complete-device)))
  (with-current-buffer (dctrl-get-buffer device-name)
    (let* ((actions dctrl-avail-actions)
	   (action (ido-completing-read "Action: " (mapcar 'car actions) nil t)))
      (nconc dctrl-actions (funcall (assoc-default action actions)))
      (dctrl-start))))

(defun dctrl-cancel (device-name)
  "Cancel the current action and pause the fifo execution."
  (interactive (list (dctrl-complete-device 'running)))
  (with-current-buffer (dctrl-get-buffer device-name)
    (unless (eq dctrl-state 'running)
      (error "device-control: selected control buffer is not in running state."))
    (dctrl-kill-current)
    (funcall (dctrl-backend-cancel dctrl-backend))
    (setf dctrl-state 'paused)
    (dctrl-warn "Actions FIFO execution canceled.")))

(defun dctrl-cancel-and-clear (&optional device-name)
  "Cancel the current action and pause the fifo execution."
  (interactive (list (dctrl-complete-device 'running)))
  (with-current-buffer (dctrl-get-buffer device-name)
    (when (or (not (eq dctrl-state 'running))
	      (= 1 (length dctrl-actions)))
      (error "device-control: selected control buffer is not in correct state."))
    (dctrl-kill-current)
    (dctrl-stop 'clear)
    (dctrl-warn "Actions FIFO execution canceled and FIFO cleared.")))

(defun dctrl-pause (&optional device-name)
  "Let finish the current and pause the fifo execution."
  (interactive (list (dctrl-complete-device 'running)))
  (with-current-buffer (dctrl-get-buffer device-name)
    (unless (eq dctrl-state 'running)
      (error "device-control: selected control buffer is not in running state."))
    (funcall (dctrl-backend-pause dctrl-backend))
    (setf dctrl-state 'paused)
    (dctrl-warn "Actions FIFO execution paused.")))

(defun dctrl-resume (&optional device-name)
  "Resume the fifo execution. See `dctrl-pause' and `dctrl-cancel'"
  (interactive (list (dctrl-complete-device 'paused)))
  (with-current-buffer (dctrl-get-buffer device-name)
    (unless (eq dctrl-state 'paused)
      (error "device-control: selected control buffer is not in paused state."))
    (funcall (dctrl-backend-resume dctrl-backend))
    (dctrl-start)))

;; Mode
(defvar device-control-mode-map nil
  "Keymap for device-control major mode.")
(unless device-control-mode-map
  (setq device-control-mode-map (make-sparse-keymap))
  (define-key device-control-mode-map "d" 'device-control)
  (define-key device-control-mode-map "c" 'dctrl-cancel)
  (define-key device-control-mode-map "C" 'dctrl-cancel-and-clear)
  (define-key device-control-mode-map "p" 'dctrl-pause)
  (define-key device-control-mode-map "r" 'dctrl-resume))

(define-derived-mode device-control-mode fundamental-mode
  "device-control"
  "device-control major mode.
Special commands:
\\{device-control-mode-map}")

;; Tools
(defun dctrl-build-fun-list (prefix)
  "Build the alist (FUNCTION_NAME . FUNCTION) of all functions
defined matching the given prefix. All get-actions() backend
functions should use this."
  (let ((fun-list (list '("a" . 'b))))
    (mapatoms (lambda (x)
		(when (and (string-prefix-p prefix (symbol-name x))
			   (symbol-function x))
		  (nconc fun-list (list (cons (substring (symbol-name x)
							 (length prefix)) x))))))
    (sort (cdr fun-list) (lambda (x y) (string< (car x) (car y))))))

(defun dctrl-process-sentinel (p e)
  (with-current-buffer (process-buffer p)
    (if (string= e "finished\n")
	(when (eq dctrl-state 'running)
	  (dctrl-continue))
      (dctrl-stop 'failure))))

(defun dctrl-process-filter (p str)
  (with-current-buffer (process-buffer p)
    (insert (replace-regexp-in-string "\r" "\n" str))))

(defmacro dctrl-internal-run-process (start-process-fun)
  `(lexical-let ((args args))
     (list (lambda ()
	     (dctrl-msg (mapconcat 'identity args " "))
	     (set-process-sentinel (apply ,start-process-fun "ctrl" (current-buffer) args)
				   'dctrl-process-sentinel)
	     (set-process-filter (get-buffer-process (current-buffer))
				 'dctrl-process-filter)
	     'sleep))))

(defun dctrl-run-process (args)
  (dctrl-internal-run-process 'start-file-process))

(defun dctrl-run-local-process (args)
  (dctrl-internal-run-process 'start-process))

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
	    cmd (list "scp" src-filename
		      (format "%s%s:%s" dst-user dst-host dst-filename))
	    final-filename dst-filename)))
    (if xfer
	(list (apply xfer (list cmd)) final-filename)
      (list nil final-filename))))

(defsubst curry (function &rest arguments)
  (lexical-let ((function function)
		(arguments arguments))
    (lambda (&rest more) (apply function (append arguments more)))))

(defmacro save-excursion-if-not-at-point-max (buf &rest body)
  (declare (indent 1))
  `(if (= (point-max) (point))
       (progn ,@body
              (when (get-buffer-window ,buf)
                (set-window-point (get-buffer-window ,buf) (point-max))))
     (save-excursion (progn ,@body))))

(provide 'device-control)
