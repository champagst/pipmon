;;; pipmon-rpc.el --- RPC protocol for pipmon -*- lexical-binding: t -*-
;;
;; Copyright (C) 2013-2019 Jorgen Schaefer <http://github.com/jorgenschaefer/elpy>
;;
;; Author: Steven Champagne
;; URL: https://github.com/champagst/pipmon
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; pipmon-rpc is a simple JSON-based RPC protocol. It's mostly JSON-RPC
;; 1.0, except we do not implement the full protocol as we do not need
;; all the features. Emacs starts a Python subprocess which runs a
;; special module. The module reads JSON-RPC requests and responds
;; with JSON-RPC responses.
;;
;;; Code:

(require 'json)
(require 'pyvenv)

(defconst pipmon-version "1.35.0"
  "The version of the pipmon lisp code.")

(defun pipmon-insert--header (&rest text)
  "Insert TEXT has a header for a buffer."
  (insert (propertize (mapconcat #'(lambda (x) x)
                                 text
                                 "")
                      'face 'header-line)
          "\n"
          "\n"))

(defun pipmon-config-error (&optional fmt &rest args)
  "Note a configuration problem.

FMT is the formating string.

This will show a message in the minibuffer that tells the user to
use \\[pipmon-config]."
  (let ((msg (if fmt
                 (apply #'format fmt args)
               "pipmon is not properly configured")))
    (error "%s; use M-x pipmon-config to configure it" msg)))

(defun pipmon-insert--para (&rest messages)
  "Insert MESSAGES, a list of strings, and then fill it."
  (let ((start (point)))
    (mapc (lambda (obj)
            (if (stringp obj)
                (insert obj)
              (insert (format "%s" obj))))
          messages)
    (fill-region start (point))))

(defun pipmon-library-root ()
  "Return the root of the Python package chain of the current buffer.

That is, if you have /foo/package/module.py, it will return /foo,
so that import package.module will pick up module.py."
  (locate-dominating-file default-directory
                          (lambda (dir)
                            (not (file-exists-p
                                  (format "%s/__init__.py"
                                          dir))))))

(defcustom pipmon-trace-chunk-size 1024
  "The maximum chunk size for each trace notification.

Spread a large trace message over a longer duration to keep emacs responsive."
  :type 'integer
  :safe #'integerp
  :group 'pipmon)

(defcustom pipmon-request-timeout 5000
  "Number of ms between each Gitlab request."
  :type 'integer
  :safe #'integerp
  :group 'pipmon)

(defcustom pipmon-trail-by 5
  "Number of pages behind tail to start trace."
  :type 'integer
  :safe #'integerp
  :group 'pipmon)

(defcustom pipmon-rpc-maximum-buffer-age (* 5 60)
  "Seconds after which pipmon automatically closes an unused RPC buffer.

pipmon creates RPC buffers over time, depending on python interpreters
and the project root. When there are many projects being worked on,
these can accumulate. Setting this variable to an integer will close
buffers and processes when they have not been used for this amount of
seconds.

Setting this variable to nil will disable the behavior."
  :type '(choice (const :tag "Never" nil)
                 integer)
  :group 'pipmon)

(defcustom pipmon-rpc-large-buffer-size 4096
  "Size for a source buffer up to which it will be sent directly.

The pipmon RPC protocol uses JSON as the serialization format.
Large buffers take a long time to encode, so pipmon can transmit
them via temporary files. If a buffer is larger than this value,
it is sent via a temporary file."
  :type 'integer
  :safe #'integerp
  :group 'pipmon)

(defcustom pipmon-rpc-ignored-buffer-size 102400
  "Size for a source buffer over which pipmon completion will not work.

To provide completion, pipmon's backends have to parse the whole
file every time. For very large files, this is slow, and can make
Emacs laggy. pipmon will simply not work on buffers larger than
this to prevent this from happening."
  :type 'integer
  :safe #'integerp
  :group 'pipmon)

(defcustom pipmon-rpc-python-command (if (equal system-type 'windows-nt)
                                       (or (executable-find "py")
                                           (executable-find "pythonw")
                                           "python")
                                     (if (executable-find "python")
					 "python"
				       ;; Fallback on systems where python is not
				       ;; symlinked to python3.
				       "python3"))
  "The Python interpreter for the RPC backend.

This should NOT be an interactive shell like ipython or jupyter.

As the RPC should be independent of any virtual environment, pipmon
will try to use the system interpreter if it exists. If you wish
to use a specific python interpreter (from a virtual environment
for example), set this to the full interpreter path."
  :type '(choice (const :tag "python" "python")
                 (const :tag "python2" "python2")
                 (const :tag "python3" "python3")
                 (const :tag "pythonw (Python on Windows)" "pythonw")
                 (const :tag "py (other Python on Windows)" "py")
                 (string :tag "Other"))
  :safe (lambda (val)
          (member val '("python" "python2" "python3" "pythonw")))
  ;; Make sure there is no obsolete rpc running
  :set (lambda (var val)                ;
         (set-default var val)
         (when (and (fboundp 'pipmon-rpc-restart)
                    (not (autoloadp #'pipmon-rpc-restart)))
           (pipmon-rpc-restart)))
  :group 'pipmon)

(defcustom pipmon-rpc-pythonpath (file-name-directory load-file-name)
  "A directory to add to the PYTHONPATH for the RPC process.

This should be a directory where the pipmon module can be found. If
this is nil, it's assumed pipmon can be found in the standard path.
Usually, there is no need to change this."
  :type 'directory
  :safe #'file-directory-p
  :group 'pipmon)

(defcustom pipmon-rpc-timeout 1
  "Number of seconds to wait for a response when blocking.

When pipmon blocks Emacs to wait for a response from the RPC
process, it will assume it won't come or wait too long after this
many seconds. On a slow computer, or if you have a large project,
you might want to increase this.

A setting of nil means to block indefinitely."
  :type '(choice (const :tag "Block indefinitely" nil)
                 integer)
  :safe (lambda (val)
          (or (integerp val)
              (null val)))
  :group 'pipmon)

(defcustom pipmon-rpc-error-timeout 30
  "Minimum number of seconds between error popups.

When pipmon encounters an error in the backend, it will display a
lengthy description of the problem for a bug report. This hangs
Emacs for a moment, and can be rather annoying if it happens
repeatedly while editing a source file.

If this variabl is non-nil, pipmon will not display the error
message again within this amount of seconds."
  :type 'integer
  :group 'pipmon)

(defvar pipmon-rpc--call-id 0
  "Call id of the last call to `pipmon-rpc`.

Used to associate responses to callbacks.")
;; (make-variable-buffer-local 'pipmon-rpc--call-id)

(defvar pipmon-rpc--buffer-p nil
  "Non-nil if the current buffer is an pipmon-rpc buffer.")
;; (make-variable-buffer-local 'pipmon-rpc--buffer-p)

(defvar pipmon-rpc--buffer nil
  "The pipmon-rpc buffer associated with this buffer.")
;; (make-variable-buffer-local 'pipmon-rpc--buffer)

(defvar pipmon-rpc--backend-library-root nil
  "The project root used by this backend.")
;; (make-variable-buffer-local 'pipmon-rpc--backend-library-root)

(defvar pipmon-rpc--backend-python-command nil
  "The Python interpreter used by this backend.")
;; (make-variable-buffer-local 'pipmon-rpc--backend-python-command)

(defvar pipmon-rpc--backend-callbacks nil
  "The callbacks registered for calls to the current backend.

This maps call IDs to functions.")
;; (make-variable-buffer-local 'pipmon-rpc--backend-callbacks)

(defvar pipmon-rpc--backend-notifications nil)

(defvar pipmon-rpc--last-call nil
  "The time of the last RPC call issued for this backend.")
;; (make-variable-buffer-local 'pipmon-rpc--last-call)

(defvar pipmon-rpc--last-error-popup nil
  "The last time an error popup happened.")

;;;;;;;;;;;;;;;;;;;
;;; RPC virualenv

(defcustom pipmon-rpc-virtualenv-path 'default
  "Path to the virtualenv used by the RPC.

Can be `default' (create a dedicated virtualenv in
\".emacs.d/pipmon\"), `system' (use the system environment),
`current' (use the currently active environment), a virtualenv
path or a function returning a virtualenv path.

If the default virtual environment does not exist, it will be
created using `pipmon-rpc-python-command' and populated with the
needed packages from `pipmon-rpc--get-package-list'."

  :type '(choice (const :tag "Dedicated environment" default)
                 (const :tag "Global environment" system)
                 (const :tag "Current environment" current)
                 (string :tag "Virtualenv path")
                 (function :tag "Function returning the virtualenv path"))
  :group 'pipmon)

(defun pipmon-rpc-default-virtualenv-path ()
  "Return the default virtualenv path."
  (expand-file-name (locate-user-emacs-file "pipmon/rpc-venv")))

(defun pipmon-rpc-get-virtualenv-path ()
  "Return the RPC virutalenv path to use."
  (cond
   ((eq pipmon-rpc-virtualenv-path 'default)
    (pipmon-rpc-default-virtualenv-path))
   ((or (eq pipmon-rpc-virtualenv-path 'system)
        (eq pipmon-rpc-virtualenv-path 'global))  ;; for backward compatibility
    (let ((exec-path (reverse exec-path)))
      (directory-file-name
       (file-name-directory
        (directory-file-name
         (file-name-directory
          (executable-find pipmon-rpc-python-command)))))))
   ((eq pipmon-rpc-virtualenv-path 'current)
    (directory-file-name
     (file-name-directory
      (directory-file-name
       (file-name-directory
        (executable-find pipmon-rpc-python-command))))))
   ((stringp pipmon-rpc-virtualenv-path)
    (expand-file-name pipmon-rpc-virtualenv-path))
   ((functionp pipmon-rpc-virtualenv-path)
    (expand-file-name (funcall pipmon-rpc-virtualenv-path)))
   (t
    (error "Invalid value for `pipmon-rpc-virtualenv-path', please set it to a proper value using customize"))))

(defun pipmon-rpc--get-pip-dependencies (python-version)
  "Return a list of RPC dependencies, based on the current PYTHON-VERSION."
  '("python-gitlab" "PyYAML"))

(defun pipmon-rpc--get-package-list ()
  "Return the list of packages to be installed in the RPC virtualenv."
  (let ((rpc-python-version (pipmon-rpc--get-python-version)))
    (pipmon-rpc--get-pip-dependencies rpc-python-version)))

(defun pipmon-rpc--get-python-version ()
  "Return the RPC python version."
  (with-temp-buffer
    (call-process pipmon-rpc-python-command nil t nil "--version")
    (goto-char (point-min))
    (re-search-forward "Python \\([0-9.]+\\)")
    (match-string 1)))

(defmacro with-pipmon-rpc-virtualenv-activated (&rest body)
  "Run BODY with pipmon's RPC virtualenv activated.

During the execution of BODY the following variables are available:
- `current-environment': current environment path.
- `current-environment-binaries': current environment python binaries path.
- `current-environment-is-deactivated': non-nil if the current
  environment has been deactivated (it is not if the RPC environment and
  the current environment are the same)."
  `(if (not (executable-find pipmon-rpc-python-command))
       (error "Cannot find executable '%s', please set 'pipmon-rpc-python-command' to an existing executable." pipmon-rpc-python-command)
     (let* ((pyvenv-post-activate-hooks (remq 'pipmon-rpc--disconnect
                                              pyvenv-post-activate-hooks))
            (pyvenv-post-deactivate-hooks (remq 'pipmon-rpc--disconnect
                                                pyvenv-post-deactivate-hooks))
            (venv-was-activated pyvenv-virtual-env)
            (current-environment-binaries (executable-find
                                           pipmon-rpc-python-command))
            (current-environment (directory-file-name (file-name-directory (directory-file-name (file-name-directory current-environment-binaries)))))
            ;; No need to change of venv if they are the same
            (same-venv (or (string= current-environment
                                    (pipmon-rpc-get-virtualenv-path))
                           (file-equal-p current-environment
                                         (pipmon-rpc-get-virtualenv-path))))
            current-environment-is-deactivated)
       ;; If different than the current one, try to activate the RPC virtualenv
       (unless same-venv
         (condition-case err
             (pyvenv-activate (pipmon-rpc-get-or-create-virtualenv))
           ((error quit) (if venv-was-activated
                             (pyvenv-activate venv-was-activated)
                           (pyvenv-deactivate))))
         (setq current-environment-is-deactivated t))
       (let (venv-err result)
         ;; Run BODY and catch errors and quit to avoid keeping the RPC
         ;; virtualenv activated
           (condition-case err
               (setq result (progn ,@body))
             (error (setq venv-err
                          (if (stringp err)
                              err
                            (car (cdr err)))))
             (quit nil))
         ;; Reactivate the previous environment if necessary
         (unless same-venv
           (if venv-was-activated
               (pyvenv-activate venv-was-activated)
             (pyvenv-deactivate)))
         ;; Raise errors that could have happened in BODY
         (when venv-err
           (error venv-err))
         result))))

(defun pipmon-rpc-get-or-create-virtualenv ()
  "Return pipmon's RPC virtualenv.

Create the virtualenv if it does not exist yet.
Update the virtualenv if the variable `pipmon-rpc-python-command' has
changed since the virtualenv creation.

An additional file `pipmon-rpc-python-path-command' is added in the
virtualenv directory in order to keep track of the python
binaries used to create the virtualenv."
  (let* ((rpc-venv-path (pipmon-rpc-get-virtualenv-path))
         (is-venv-exist (file-exists-p rpc-venv-path))
         (is-default-rpc-venv
          (and rpc-venv-path
               (string= rpc-venv-path
                        (pipmon-rpc-default-virtualenv-path))))
         (venv-python-path-command-file
          (concat (file-name-as-directory rpc-venv-path)
                  "pipmon-rpc-python-path-command"))
         (venv-python-path-command
          (when (file-exists-p venv-python-path-command-file)
            (with-temp-buffer
              (insert-file-contents venv-python-path-command-file)
              (buffer-string))))
         (venv-need-update
          (and is-venv-exist
               is-default-rpc-venv
               (not (string= venv-python-path-command
                             pipmon-rpc-python-command))))
         (venv-creation-allowed
          (and (or (not is-venv-exist) venv-need-update)
               (or is-default-rpc-venv
                   (y-or-n-p
                    (format "`pipmon-rpc-virtualenv-path' was set to '%s', but this virtualenv does not exist, create it ? " rpc-venv-path))))))
    ;; Delete the rpc virtualenv if obsolete
    (when venv-need-update
      (delete-directory rpc-venv-path t)
      (setq is-venv-exist nil))
    ;; Create a new rpc venv if necessary
    (unless is-venv-exist
      (if (not venv-creation-allowed)
          (message "Please indicate the virtualenv you wish to use with `pipmon-rpc-virtualenv-path'.")
        (let ((deact-venv pyvenv-virtual-env))
          (message "pipmon is %s the RPC virtualenv ('%s')"
                   (if venv-need-update "updating" "creating")
                   rpc-venv-path)
          (pipmon-rpc--create-virtualenv rpc-venv-path)
          ;; Make sure the rpc venv is deacivated on quit
          (condition-case nil
              (progn
                (pyvenv-activate rpc-venv-path)
                ;; Add file to keep track of the `pipmon-rpc-python-command` used
                (with-temp-file venv-python-path-command-file
                  (insert pipmon-rpc-python-command))
                ;; safeguard to be sure we don't install stuff in the wrong venv
                (when (file-equal-p pyvenv-virtual-env rpc-venv-path)
                  (pipmon-rpc--install-dependencies))
                (pipmon-rpc-restart))
            (quit nil))
          ;; Deactivate the rpc venv
          (if deact-venv
              (pyvenv-activate (directory-file-name deact-venv))
            (pyvenv-deactivate))
          (message "Done"))))
    rpc-venv-path))

(defun pipmon-rpc--create-virtualenv (rpc-venv-path)
  "Create a virtualenv for the RPC in RPC-VENV-PATH."
  ;; venv cannot create a proper virtualenv from inside another virtualenv
  (let* ((pipmon-rpc-virtualenv-path 'system)
         success
         (pipmon-venv-buffname-visible "*pipmon-virtualenv*")
         (pipmon-venv-buffname (concat " " pipmon-venv-buffname-visible)))
    (when (get-buffer pipmon-venv-buffname)
      (kill-buffer pipmon-venv-buffname))
    (when (get-buffer pipmon-venv-buffname-visible)
      (kill-buffer pipmon-venv-buffname-visible))
    (with-pipmon-rpc-virtualenv-activated
     (cond
      ((and (= 0 (call-process pipmon-rpc-python-command nil nil nil
                              "-m" "venv" "-h"))
           ;; see https://github.com/jorgenschaefer/elpy/issues/1756
           (= 0 (call-process pipmon-rpc-python-command nil nil nil
                              "-m" "ensurepip" "-h")))
       (with-current-buffer (get-buffer-create pipmon-venv-buffname)
         (insert (concat "Running '" pipmon-rpc-python-command " -m venv "
                         rpc-venv-path "':\n\n"))
         (setq success (call-process pipmon-rpc-python-command nil t t
                                     "-m" "venv" rpc-venv-path))))
      ((executable-find "virtualenv")
       (with-current-buffer (get-buffer-create pipmon-venv-buffname)
         (insert (concat "Running 'virtualenv -p "
                         pipmon-rpc-python-command " " rpc-venv-path
                         "':\n\n"))
         (setq success (call-process "virtualenv" nil t t "-p"
                                     pipmon-rpc-python-command rpc-venv-path))))
      (t
       (error "pipmon needs the 'virtualenv' or 'venv' python packages to create its virtualenv. Please install one of them or disable the dedicated virtualenv with `(setq pipmon-rpc-virtualenv-path 'current)`"))))
    ;; warn us if something wrong happened
    (unless (= 0 success)
      (with-current-buffer pipmon-venv-buffname
        (rename-buffer pipmon-venv-buffname-visible)
        (goto-char (point-max))
        (insert
         (concat
          "\n\n"
          "pipmon failed to install its dedicated virtualenv due to the above\n"
          "error. If the error details does not help you fixing it, You can\n"
          "report this problem on pipmon repository on github.\n"
          "In the meantime, setting the `pipmon-rpc-virtualenv-path' option to\n"
          "either `system' or `current' should temporarily fix the issue.")))
      (error (concat "pipmon failed to create its dedicated virtualenv. "
                     "Please check the `" pipmon-venv-buffname-visible
                     "' buffer.")))))

(defun pipmon-rpc--install-dependencies ()
  "Install the RPC dependencies in the current virtualenv."
  (if (y-or-n-p "Automatically install the RPC dependencies from PyPI (needed for gitlab and yaml) ? ")
      (with-temp-buffer
        (message "pipmon is installing the RPC dependencies...")
        (when (/= (apply 'call-process pipmon-rpc-python-command
                         nil t nil
                         "-m" "pip" "install" "--upgrade"
                         (pipmon-rpc--get-package-list))
                  0)
          (message "pipmon failed to install some of the RPC dependencies, please use `pipmon-config' to install them.\n%s" (buffer-string))
          ))
    (message "Some of pipmon's functionnalities will not work, please use `pipmon-config' to install the needed python dependencies.")))

(defun pipmon-rpc-reinstall-virtualenv ()
  "Re-install the RPC virtualenv."
  (interactive)
  (let ((rpc-venv-path (pipmon-rpc-get-virtualenv-path)))
    (when
        (cond
         ((or (eq pipmon-rpc-virtualenv-path 'system)
              (eq pipmon-rpc-virtualenv-path 'global)) ;; backward compatibility
          (error "Cannot reinstall the system environment, please reinstall the necessary packages manually"))
         ((string= (pipmon-rpc-default-virtualenv-path) rpc-venv-path)
          t)
         (t
          (y-or-n-p (format "Are you sure you want to reinstall the virtualenv in '%s' (every manual modifications will be lost) ? " rpc-venv-path))))
      (delete-directory rpc-venv-path t)
      (pipmon-rpc-get-or-create-virtualenv))))

(defun pipmon-rpc--pip-missing ()
  "Return t if pip is not installed in the RPC virtualenv."
  (let* ((rpc-venv-path (file-name-as-directory
                         (pipmon-rpc-get-virtualenv-path)))
         (base-pip-scripts (concat rpc-venv-path
                                   (file-name-as-directory "Scripts")
                                   "pip"))
         (base-pip-bin (concat rpc-venv-path
                               (file-name-as-directory "bin")
                               "pip")))
    (not (or
          (file-exists-p base-pip-scripts)
          (file-exists-p base-pip-bin)
          (file-exists-p (concat base-pip-scripts ".exe"))
          (file-exists-p (concat base-pip-bin ".exe"))))))

;;;;;;;;;;;;;;;;;;;
;;; Promise objects

(defvar pipmon-promise-marker (make-symbol "*pipmon-promise*")
  "An uninterned symbol marking an pipmon promise object.")

(defun pipmon-promise (success &optional error)
  "Return a new promise.

A promise is an object with a success and error callback. If the
promise is resolved using `pipmon-promise-resolve', the SUCCESS
callback is called with the given value. The current buffer is
restored, too.

If the promise is rejected using `pipmon-promise-reject', the ERROR
callback is called. For this function, the current buffer is not
necessarily restored, as it is also called when the buffer does
not exist anymore."
  (vector pipmon-promise-marker ; 0 id
          success             ; 1 success-callback
          error               ; 2 error-callback
          (current-buffer)    ; 3 current-buffer
          nil                 ; 4 run
          ))

(defun pipmon-promise-p (obj)
  "Return non-nil if OBJ is a promise object."
  (and (vectorp obj)
       (= (length obj) 5)
       (eq (aref obj 0) pipmon-promise-marker)))

(defsubst pipmon-promise-success-callback (promise)
  "Return the success callback for PROMISE."
  (aref promise 1))

(defsubst pipmon-promise-error-callback (promise)
  "Return the error callback for PROMISE."
  (aref promise 2))

(defsubst pipmon-promise-buffer (promise)
  "Return the buffer for PROMISE."
  (aref promise 3))

(defsubst pipmon-promise-resolved-p (promise)
  "Return non-nil if the PROMISE has been resolved or rejected."
  (aref promise 4))

(defsubst pipmon-promise-set-resolved (promise)
  "Mark PROMISE as having been resolved."
  (aset promise 4 t))

(defun pipmon-promise-resolve (promise value)
  "Resolve PROMISE with VALUE."
  (unless (pipmon-promise-resolved-p promise)
    (unwind-protect
        (let ((success-callback (pipmon-promise-success-callback promise)))
          (when success-callback
            (condition-case err
                (with-current-buffer (pipmon-promise-buffer promise)
                  (funcall success-callback value))
              (error
               (pipmon-promise-reject promise err)))))
      (pipmon-promise-set-resolved promise))))

(defun pipmon-promise-reject (promise reason)
  "Reject PROMISE because of REASON."
  (unless (pipmon-promise-resolved-p promise)
    (unwind-protect
        (let ((error-callback (pipmon-promise-error-callback promise)))
          (when error-callback
            (if (buffer-live-p (pipmon-promise-buffer promise))
                (with-current-buffer (pipmon-promise-buffer promise)
                  (funcall error-callback reason))
              (with-temp-buffer
                (funcall error-callback reason)))))
      (pipmon-promise-set-resolved promise))))

(defun pipmon-promise-wait (promise &optional timeout)
  "Wait for PROMISE to be resolved, for up to TIMEOUT seconds.

This will accept process output while waiting.

This will wait for the current pipmon RPC process specifically, as
Emacs currently has a bug where it can wait for the entire time
of the timeout, even if output arrives.

See http://debbugs.gnu.org/cgi/bugreport.cgi?bug=17647"
  (let ((end-time (when timeout
                    (time-add (current-time)
                              (seconds-to-time timeout))))
        (process (get-buffer-process (pipmon-rpc--get-rpc-buffer))))
    (while (and (not (pipmon-promise-resolved-p promise))
                (or (not end-time)
                    (time-less-p (current-time)
                                 end-time)))
      (accept-process-output process timeout))))

;;;;;;;;;;;;;;;;;
;;; pipmon RPC

(defun pipmon-rpc (method params &optional success error)
  "Call METHOD with PARAMS in the backend.

If SUCCESS and optionally ERROR is given, return immediately and
call those when a result is available. Otherwise, wait for a
result and return that."
  (unless error
    (setq error #'pipmon-rpc--default-error-callback))
  (if success
      (pipmon-rpc--call method params success error)
    (pipmon-rpc--call-blocking method params)))

(defun pipmon-rpc--call-blocking (method-name params)
  "Call METHOD-NAME with PARAMS in the current RPC backend.

Returns the result, blocking until this arrived."
  (let* ((result-arrived nil)
         (error-occured nil)
         (result-value nil)
         (error-object nil)
         (promise (pipmon-rpc--call method-name params
                                  (lambda (result)
                                    (setq result-value result
                                          result-arrived t))
                                  (lambda (err)
                                    (setq error-object err
                                          error-occured t)))))
    (pipmon-promise-wait promise pipmon-rpc-timeout)
    (cond
     (error-occured
      (pipmon-rpc--default-error-callback error-object))
     (result-arrived
      result-value)
     (t
      (error "Timeout during RPC call %s from backend"
             method-name)))))

(defun pipmon-rpc--call (method-name params success error)
  "Call METHOD-NAME with PARAMS in the current RPC backend.

When a result is available, SUCCESS will be called with that
value as its sole argument. If an error occurs, ERROR will be
called with the error list.

Returns a PROMISE object."
  (let ((promise (pipmon-promise success error)))
    (with-current-buffer (pipmon-rpc--get-rpc-buffer)
      (setq pipmon-rpc--call-id (1+ pipmon-rpc--call-id)
            pipmon-rpc--last-call (float-time))
      (pipmon-rpc--register-callback pipmon-rpc--call-id promise)
      (process-send-string
       (get-buffer-process (current-buffer))
       (let ((json-encoding-pretty-print nil))  ;; Link to bug https://github.com/jorgenschaefer/elpy/issues/1521
         (concat (json-encode `((id . ,pipmon-rpc--call-id)
                                (method . ,method-name)
                                (params . ,params)))
                 "\n"))))
    promise))

(defun pipmon-rpc--register-callback (call-id promise)
  "Register for PROMISE to be called when CALL-ID returns.

Must be called in an pipmon-rpc buffer."
  (unless pipmon-rpc--buffer-p
    (error "Must be called in RPC buffer"))
  (unless pipmon-rpc--backend-callbacks
    (setq pipmon-rpc--backend-callbacks (make-hash-table :test #'equal)))
  (puthash call-id promise pipmon-rpc--backend-callbacks))

(defun pipmon-rpc--get-rpc-buffer ()
  "Return the RPC buffer associated with the current buffer,
creating one if necessary."
  (unless (pipmon-rpc--process-buffer-p pipmon-rpc--buffer)
    (setq pipmon-rpc--buffer
          (or (pipmon-rpc--find-buffer (pipmon-library-root)
                                     pipmon-rpc-python-command)
              (pipmon-rpc--open (pipmon-library-root)
                              pipmon-rpc-python-command))))
  pipmon-rpc--buffer)

(defun pipmon-rpc--process-buffer-p (buffer)
  "Return non-nil when BUFFER is a live pipmon-rpc process buffer.

If BUFFER is a buffer for an pipmon-rpc process, but the process
died, this will kill the process and buffer."
  (cond
   ((or (not buffer)
        (not (buffer-live-p buffer)))
    nil)
   ((not (buffer-local-value 'pipmon-rpc--buffer-p buffer))
    nil)
   ((and (get-buffer-process buffer)
         (process-live-p (get-buffer-process buffer)))
    t)
   (t
    (ignore-errors
      (kill-process (get-buffer-process buffer)))
    (ignore-errors
      (kill-buffer buffer))
    nil)))

(defun pipmon-rpc--find-buffer (library-root python-command)
  "Return an existing RPC buffer for this project root and command."
  (catch 'return
    (let ((full-python-command (executable-find python-command)))
      (dolist (buf (buffer-list))
        (when (and (pipmon-rpc--process-buffer-p buf)
                   (equal (buffer-local-value 'pipmon-rpc--backend-library-root
                                              buf)
                          library-root)
                   (equal (buffer-local-value 'pipmon-rpc--backend-python-command
                                              buf)
                          full-python-command))
          (throw 'return buf))))
    nil))

(defun pipmon-rpc--open (library-root python-command)
  "Start a new RPC process and return the associated buffer."
  (pipmon-rpc--cleanup-buffers)
  (with-pipmon-rpc-virtualenv-activated
   (let* ((full-python-command (executable-find python-command))
          (name (format " *pipmon-rpc [project:%s environment:%s]*"
                        library-root
                        current-environment))
          (new-pipmon-rpc-buffer (generate-new-buffer name))
          (proc nil))
     (unless full-python-command
       (error "Can't find Python command, configure `pipmon-rpc-python-command'"))
     (message "Creating new buffer: %s" name)
     (with-current-buffer new-pipmon-rpc-buffer
       (setq pipmon-rpc--buffer-p t
             pipmon-rpc--buffer (current-buffer)
             pipmon-rpc--backend-library-root library-root
             pipmon-rpc--backend-python-command full-python-command
             default-directory "/"
             proc (condition-case err
                      (let ((process-connection-type nil)
                            (process-environment (append (pipmon-rpc--environment)
							 (list
							  (format "PIPMON_TRACE_CHUNK_SIZE=%d" pipmon-trace-chunk-size)
							  (format "PIPMON_REQUEST_TIMEOUT=%d" pipmon-request-timeout)
							  (format "PIPMON_TRAIL_BY=%d" pipmon-trail-by)))))
                        (start-process name
                                       (current-buffer)
                                       full-python-command
                                       "-W" "ignore"
                                       "-m" "pipmon.__main__"))
                    (error
                     (pipmon-config-error
                      "pipmon can't start Python (%s: %s)"
                      (car err) (cadr err)))))
       (set-process-query-on-exit-flag proc nil)
       (set-process-sentinel proc #'pipmon-rpc--sentinel)
       (set-process-filter proc #'pipmon-rpc--filter))
     new-pipmon-rpc-buffer)))

(defun pipmon-rpc--cleanup-buffers ()
  "Close RPC buffers that have not been used in five minutes."
  (when pipmon-rpc-maximum-buffer-age
    (let ((old (- (float-time)
                  pipmon-rpc-maximum-buffer-age)))
      (dolist (buffer (buffer-list))
        (when (and (pipmon-rpc--process-buffer-p buffer)
                   (< (or (buffer-local-value 'pipmon-rpc--last-call buffer)
                          old)
                      old))
          (ignore-errors
            (kill-process (get-buffer-process buffer)))
          (ignore-errors
            (kill-buffer buffer)))))))

(defun pipmon-rpc--sentinel (process event)
  "The sentinel for the RPC process.

As process sentinels are only ever called when the process
terminates, this will call the error handler of all registered
RPC calls with the event."
  (let ((buffer (process-buffer process))
        (err (list 'process-sentinel (substring event 0 -1))))
    (when (and buffer
               (buffer-live-p buffer))
      (with-current-buffer buffer
        (when pipmon-rpc--backend-callbacks
          (maphash (lambda (_call-id promise)
                     (ignore-errors
                       (pipmon-promise-reject promise err)))
                   pipmon-rpc--backend-callbacks)
          (setq pipmon-rpc--backend-callbacks nil))))))

(defun pipmon-rpc--filter (process output)
  "The filter for the RPC process."
  (let ((buffer (process-buffer process)))
    (when (and buffer
               (buffer-live-p buffer))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert output)
        (while (progn
                 (goto-char (point-min))
                 (search-forward "\n" nil t))
          (let ((line-end (point))
                (json nil)
                (did-read-json nil))
            (goto-char (point-min))
            (condition-case _err
                (progn
                  (setq json (let ((json-array-type 'list)
                                   (json-false nil)
                                   (json-encoding-pretty-print nil))  ;; Link to bug https://github.com/jorgenschaefer/elpy/issues/1521
                               (json-read)))
                  (if (listp json)
                      (setq  line-end (1+ (point))
                             did-read-json t)
                    (goto-char (point-min))))
              (error
               (goto-char (point-min))))
            (cond
             (did-read-json
              (delete-region (point-min) line-end)
              (pipmon-rpc--handle-json json))
             ((looking-at "pipmon-rpc ready\n")
              (replace-match "")
              (pipmon-rpc--check-backend-version "1.1"))
             ((looking-at "pipmon-rpc ready (\\([^ ]*\\))\n")
              (let ((rpc-version (match-string 1)))
                (replace-match "")
                (pipmon-rpc--check-backend-version rpc-version)))
             ((looking-at ".*No module named pipmon\n")
              (replace-match "")
              (pipmon-config-error "pipmon module not found"))
             (t
              (let ((line (buffer-substring (point-min)
                                            line-end)))
                (delete-region (point-min) line-end)
                (pipmon-rpc--handle-unexpected-line line))))))))))

(defmacro pipmon-insert--popup (buffer-name &rest body)
  "Pop up a help buffer named BUFFER-NAME and execute BODY in it."
  (declare (indent 1))
  `(with-help-window ,buffer-name
     (with-current-buffer standard-output
       ,@body)))

(defun pipmon-rpc--check-backend-version (rpc-version)
  "Check that we are using the right version."
  (unless (equal rpc-version pipmon-version)
    (pipmon-insert--popup "*pipmon Version Mismatch*"
      (pipmon-insert--header "pipmon Version Mismatch")
      (pipmon-insert--para
       "You are not using the same version of pipmon in Emacs Lisp "
       "compared to Python. This can cause random problems. Please "
       "do make sure to use compatible versions.\n\n"
       "This often happens because you have an obsolete pipmon python "
       "package installed on your system/virtualenv. This package "
       "shadows the pipmon python package shipped with pipmon, leading "
       "to this mismatch. If it is the case, uninstalling the pipmon "
       "python package (with pip for example) should resolve the issue.\n")
      (insert
       "\n"
       "pipmon Emacs Lisp version: " pipmon-version "\n"
       "pipmon Python version....: " rpc-version "\n"))))

(defun pipmon-rpc--handle-unexpected-line (line)
  "Handle an unexpected line from the backend.

This is usually an error or backtrace."
  (let ((buf (get-buffer "*pipmon Output*")))
    (unless buf
      (pipmon-insert--popup "*pipmon Output*"
        (pipmon-insert--header "Output from Backend")
        (pipmon-insert--para
         "There was some unexpected output from the pipmon backend. "
         "This is usually not a problem and should usually not be "
         "reported as a bug with pipmon. You can safely hide this "
         "buffer and ignore it. You can also see the output below "
         "in case there is an actual problem.\n\n")
        (pipmon-insert--header "Output")
        (setq buf (current-buffer))))
    (with-current-buffer buf
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert line)))))

(defun pipmon-rpc--handle-json (json)
  "Handle a single JSON object from the RPC backend."
  (let ((call-id (cdr (assq 'id json)))
        (error-object (cdr (assq 'error json)))
        (result (cdr (assq 'result json))))
    (if (null call-id)
	(progn
	  (unless (null pipmon-rpc--backend-notifications)
	    (funcall pipmon-rpc--backend-notifications result)))
      (let ((promise (gethash call-id pipmon-rpc--backend-callbacks)))
	(unless promise
	  (error "Received a response for unknown call-id %s" call-id))
	(remhash call-id pipmon-rpc--backend-callbacks)
	(if error-object
	    (pipmon-promise-reject promise error-object)
	  (pipmon-promise-resolve promise result))))))

(defun pipmon-rpc--default-error-callback (error-object)
  "Display an error from the RPC backend."
  ;; We actually might get an (error "foo") thing here.
  (if (and (consp error-object)
           (not (consp (car error-object))))
      (signal (car error-object) (cdr error-object))
    (let ((message (cdr (assq 'message error-object)))
          (code (cdr (assq 'code error-object)))
          (data (cdr (assq 'data error-object))))
      (cond
       ((not (numberp code))
        (error "Bad response from RPC: %S" error-object))
       ((< code 300)
        (message "pipmon warning: %s" message))
       ((< code 500)
        (error "pipmon error: %s" message))
       ((and pipmon-rpc-error-timeout
             pipmon-rpc--last-error-popup
             (<= (float-time)
                 (+ pipmon-rpc--last-error-popup
                    pipmon-rpc-error-timeout)))
        (message "pipmon error popup ignored, see `pipmon-rpc-error-timeout': %s"
                 message))
       (t
          (pipmon-insert--popup "*pipmon Error*"
            (pipmon-insert--header "pipmon Error")
            (pipmon-insert--para
             "The backend encountered an unexpected error. This indicates "
             "a bug in pipmon. Please open a bug report with the data below "
             "in the pipmon bug tracker:")
            (insert "\n"
                    "\n")
            (insert-button
             "https://github.com/champagst/pipmon/issues/new"
             'action (lambda (button)
                       (browse-url (button-get button 'url)))
             'url "https://github.com/champagst/pipmon/issues/new")
            (insert "\n"
                    "\n"
                    "```\n")
            (pipmon-insert--header "Error Message")
            (insert message "\n\n")
            (let ((traceback (cdr (assq 'traceback data))))
              (when traceback
                (insert "\n")
                (pipmon-insert--header "Traceback")
                (insert traceback)))
            (unless (= 0 (current-column))
              (insert "\n"))
            (insert "```"))
          (setq pipmon-rpc--last-error-popup (float-time)))))))

(defun pipmon-rpc--environment ()
  "Return a `process-environment' for the RPC process.

This includes `pipmon-rpc-pythonpath' in the PYTHONPATH, if set."
  (if (or (not pipmon-rpc-pythonpath)
          (not (file-exists-p (expand-file-name "pipmon/__init__.py"
                                                pipmon-rpc-pythonpath))))
      process-environment
    (let* ((old-pythonpath (getenv "PYTHONPATH"))
           (new-pythonpath (if old-pythonpath
                               (concat pipmon-rpc-pythonpath
                                       path-separator
                                       old-pythonpath)
                             pipmon-rpc-pythonpath)))
      (cons (concat "PYTHONPATH=" new-pythonpath)
            (append process-environment
                    (when (and (string-equal system-type "windows-nt")
                               (>= (string-match-p
                                    (regexp-quote "utf-8")
                                    (format "%s" buffer-file-coding-system))) 0)
                      (list
                       "PYTHONIOENCODING=utf-8"
                       "PYTHONLEGACYWINDOWSSTDIO=1")))))))

(defun pipmon-rpc--buffer-contents ()
  "Return the contents of the current buffer.

This returns either a string, or a file object for the RPC
protocol if the buffer is larger than
`pipmon-rpc-large-buffer-size'."
  (if (< (buffer-size) pipmon-rpc-large-buffer-size)
      (buffer-string)
    (let ((file-name (make-temp-file "pipmon-rpc-"))
          (coding-system-for-write 'utf-8))
      (write-region nil nil file-name nil :nomessage)
      `((filename . ,file-name)
        (delete_after_use . t)))))

(defun pipmon-rpc--region-contents ()
  "Return the selected region as a string."
  (if (use-region-p)
      (buffer-substring (region-beginning) (region-end))))

(defun pipmon-rpc--disconnect ()
  "Disconnect rpc process from pipmon buffers."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when pipmon-mode
        (setq pipmon-rpc--buffer nil)))))

;; RPC API functions

(defun pipmon-rpc-restart ()
  "Restart all RPC processes."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (pipmon-rpc--process-buffer-p buffer)
      (ignore-errors
        (kill-process (get-buffer-process buffer)))
      (ignore-errors
        (kill-buffer buffer)))))


;;;;;;;;;;;;;;
;;; RPC API

(defun pipmon-rpc-pipeline (project-id pipeline-id &optional success error)
  (pipmon-rpc "pipeline" (list project-id pipeline-id) success error))

(defun pipmon-rpc-trigger (project-id ref variables &optional success error)
  (pipmon-rpc "trigger_pipeline" (list project-id ref variables) success error))

(defun pipmon-rpc-cancel-pipeline (project-id pipeline-id &optional success error)
  (pipmon-rpc "cancel_pipeline" (list project-id pipeline-id) success error))

(defun pipmon-rpc-retry-pipeline (project-id pipeline-id &optional success error)
  (pipmon-rpc "retry_pipeline" (list project-id pipeline-id) success error))

(defun pipmon-rpc-retry-job (project-id job-id &optional success error)
  (pipmon-rpc "retry_job" (list project-id job-id) success error))

(defun pipmon-rpc-job-trace (project-id job-id &optional success error)
  (pipmon-rpc "job_trace" (list project-id job-id) success error))

(defun pipmon-rpc-pause-trace (&optional success error)
  (message "pause")
  (pipmon-rpc "pause_trace" nil success error))

(defun pipmon-rpc-resume-trace (&optional success error)
  (pipmon-rpc "resume_trace" nil success error))

(defun pipmon-rpc-quit-trace (&optional success error)
  (pipmon-rpc "quit_trace" nil success error))

(defun pipmon-rpc-get-commit (project-id commit-id &optional success error)
  (pipmon-rpc "get_commit" (list project-id commit-id) success error))

(provide 'pipmon-rpc)
;;; pipmon-rpc.el ends here
