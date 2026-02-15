;;; pipmon.el --- A major mode for triggering and monitoring Gitlab pipelines -*- lexical-binding: t -*-

;; Copyright (C) 2026  Steven Champagne

;; Author: Steven Champagne <champagst@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/champagst/pipmon
;; Keywords: gitlab
;; Package-Requires: ((emacs "29.0") (dash) (pyvenv))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

(require 'dash)
(require 'pipmon-rpc)
(require 'cl-lib)
(require 'ansi-color)

(defvar pipmon--check (propertize "/" 'font-lock-face '(:foreground "green")))
(defvar pipmon--arrow (propertize ">" 'font-lock-face '(:foreground "blue")))
(defvar pipmon--up (propertize "!" 'font-lock-face '(:foreground "orange")))
(defvar pipmon--cross (propertize "x" 'font-lock-face '(:foreground "red")))
(defvar pipmon--pause (propertize "=" 'font-lock-face '(:foreground "blue")))
(defvar pipmon--question (propertize "?" 'font-lock-face '(:foreground "black")))

(defvar pipmon--status-symbols (list (cons "passed" pipmon--check)
				     (cons "success" pipmon--check)
				     (cons "created" pipmon--up)
				     (cons "running" pipmon--arrow)
				     (cons "failed" pipmon--cross)
				     (cons "canceled" pipmon--cross)
				     (cons "blocked" pipmon--pause)))

(defvar pipmon--complete-statuses '("blocked" "passed" "failed" "canceled"))
(defvar pipmon--saved-directory "~/.pipmon")

(cl-defstruct (pipmon-pipeline (:constructor pipmon-pipeline--create))
  id pipeline-id status name web-url jobs note)

(cl-defstruct (pipmon-job (:constructor pipmon-job--create))
  id pipeline-id job-id name status)

(cl-defstruct (pipmon-trace (:constructor pipmon-trace--create))
  consumed)

(defvar pipmon--pipelines nil)
(defvar pipmon--running-trace nil)

(setq pipmon-rpc--backend-notifications 'pipmon--handle-notification)

(defun pipmon--insert-line (obj write-fn &optional parent)
  (save-excursion
    (goto-char (point-max))
    (let ((inhibit-read-only t)
	  (start (point)))
      (insert (funcall write-fn obj))
      (let* ((overlay (make-overlay start (point) nil t)))
	(when parent
	  (when (overlay-get parent 'collapsed)
	    (overlay-put overlay 'invisible t))
	  (overlay-put parent 'children (cons overlay (overlay-get parent 'children))))
	(overlay-put overlay 'obj obj)
	(overlay-put overlay 'write-fn write-fn)
	(overlay-put overlay 'children nil)
	overlay))))

(defun pipmon--redraw (overlay)
  (save-excursion
    (let* ((inhibit-read-only t)
	   (start (overlay-start overlay))
	   (text (funcall (overlay-get overlay 'write-fn) (overlay-get overlay 'obj))))
      (goto-char start)
      (delete-region start (overlay-end overlay))
      (insert text)
      (move-overlay overlay start (+ start (length text)) nil))))

(defun pipmon--toggle-collapsed (overlay)
  (let ((invisible (not (overlay-get overlay 'collapsed))))
    (dolist (child (overlay-get overlay 'children))
      (overlay-put child 'invisible invisible))
    (overlay-put overlay 'collapsed invisible)))

(defun pipmon--status-symbol (text)
  (or (assoc-default text pipmon--status-symbols) pipmon--question))

(defun pipmon--write-pipeline (pipeline)
  (format "%s %s\n" (pipmon--status-symbol (pipmon-pipeline-status pipeline)) (pipmon-pipeline-note pipeline)))

(defun pipmon--write-job (job)
  (format "      %s %s\n" (pipmon--status-symbol (pipmon-job-status job)) (pipmon-job-name job)))

(defun pipmon--insert-job (job parent)
  (let ((overlay (pipmon--insert-line job #'pipmon--write-job parent)))
    (overlay-put overlay 'keymap pipmon-job-map)
    overlay))

(defun pipmon--at-pipeline-p ()
  (when-let (overlay (car (overlays-at (point))))
    (when (pipmon-pipeline-p (overlay-get overlay 'obj))
      overlay)))

(defun pipmon--at-job-p ()
  (when-let (overlay (car (overlays-at (point))))
    (when (pipmon-job-p (overlay-get overlay 'obj))
      overlay)))

(defun pipmon--delete--pipeline (pipeline)
  (-when-let ((pipeline overlay) (pipmon--find-pipeline
				  (pipmon-pipeline-id pipeline)
				  (pipmon-pipeline-pipeline-id pipeline)))
    (let ((inhibit-read-only t))
      (dolist (child (overlay-get overlay 'children))
	(delete-region (overlay-start child) (overlay-end child))
	(delete-overlay child))
      (setq pipmon--pipelines (remq pipeline pipmon--pipelines))
      (delete-region (overlay-start overlay) (overlay-end overlay))
      (delete-overlay overlay)
      (pipmon-save))))

(defun pipmon--insert-pipeline (pipeline)
  (goto-char (point-max))
  (let ((overlay (pipmon--insert-line pipeline #'pipmon--write-pipeline)))
    (overlay-put overlay 'collapsed t)
    (overlay-put overlay 'keymap pipmon-pipeline-map)
    overlay))

(defun pipmon--is-pipeline-p (pipeline id pipeline-id)
  (and (pipmon-pipeline-p pipeline)
       (= (pipmon-pipeline-id pipeline) id)
       (= (pipmon-pipeline-pipeline-id pipeline) pipeline-id)))

(defun pipmon--find-pipeline-overlay (id pipeline-id)
  (with-current-buffer (pipmon-buffer)
    (seq-find (lambda (overlay)
		(pipmon--is-pipeline-p (overlay-get overlay 'obj) id pipeline-id))
	      (overlays-in (point-min) (point-max)))))

(defun pipmon--find-pipeline (id pipeline-id)
  (when-let (overlay (pipmon--find-pipeline-overlay id pipeline-id))
    (list
     (overlay-get overlay 'obj)
     overlay)))

(defun pipmon--is-job-p (job id pipeline-id job-id)
  (and (pipmon-job-p job)
       (= (pipmon-job-id job) id)
       (= (pipmon-job-pipeline-id job) pipeline-id)
       (= (pipmon-job-job-id job) job-id)))

(defun pipmon--find-job-overlay (id pipeline-id job-id)
  (with-current-buffer (pipmon-buffer)
    (seq-find (lambda (overlay)
		(pipmon--is-job-p (overlay-get overlay 'obj) id pipeline-id job-id))
	      (overlays-in (point-min) (point-max)))))

(defun pipmon--find-job (id pipeline-id job-id)
  (when-let (overlay (pipmon--find-job-overlay id pipeline-id job-id))
    (list
     (overlay-get overlay 'obj)
     overlay)))

(defun pipmon--handle-notification (result)
  (with-current-buffer (pipmon-buffer)
    (let ((source (alist-get 'source result))
	  (payload (alist-get 'payload result)))
      (cond
       ((string= source "pipeline")
	(let-alist payload
	  ;; Pipeline already exists in the list.
	  (-if-let ((pipeline overlay) (pipmon--find-pipeline .project_id .id))
	      (progn
		(setf
		 (pipmon-pipeline-status pipeline) (downcase .status)
		 (pipmon-pipeline-name pipeline) .name
		 (pipmon-pipeline-web-url pipeline) .web_url)
		(pipmon--redraw overlay))

	    ;; Add a line.
	    (let ((pipeline (pipmon-pipeline--create :id .project_id :pipeline-id .id :name .name :status (downcase .status) :web-url .web_url :note .id)))
	      (setq pipmon--pipelines (cons pipeline pipmon--pipelines))
	      (pipmon--insert-pipeline pipeline))))
	(pipmon-save))

       ((string= source "job")
	(let-alist payload
	  (-when-let ((pipeline parent) (pipmon--find-pipeline .project_id .pipeline_id))
	    ;; Job already exists in the list.
	    (-if-let ((job overlay) (pipmon--find-job .project_id .pipeline_id .job_id))
		(progn
		  (setf (pipmon-job-status job) (downcase .status))
		  (pipmon--redraw overlay))

	      ;; Add a line.
	      (let ((job (pipmon-job--create :id .project_id :pipeline-id .pipeline_id :job-id .job_id :name .name :status (downcase .status))))
		(setf (pipmon-pipeline-jobs pipeline) (cons job (pipmon-pipeline-jobs pipeline)))
		(pipmon--insert-job job parent)))))
	(pipmon-save))

       ((string= source "trace")
	(with-current-buffer (pipmon-log-buffer)
	  (let ((trace-point (pipmon-trace-consumed pipmon--running-trace)))
	    (goto-char (point-max))
	    (insert payload)
	    (search-backward "\n" nil t)
	    (replace-regexp-in-region "section_\\(start\\|end\\):[[:digit:]]+:[[:word:]_]+" "" trace-point (point))
	    (replace-regexp-in-region "\r" "" trace-point (point))
	    (ansi-color-apply-on-region trace-point (point))
	    (setf (pipmon-trace-consumed pipmon--running-trace) (point)))))

       (t
	(dolist (res result)
	  (pipmon--handle-notification res)))))))

(defun pipmon--pipeline-with-note (note)
  (lambda (result)
    (let-alist (alist-get 'payload result)
      (-when-let ((pipeline overlay) (pipmon--find-pipeline .project_id .id))
	(when note
	  (setf (pipmon-pipeline-note pipeline) note))))
    result))

(defun pipmon--plist-to-alist (plist)
  (cl-assert (cl-evenp (length plist)))
  (--map (apply 'cons it) (-partition 2 plist)))

(cl-defun pipmon-trigger (id ref &key note vars)
  (pipmon-rpc-trigger
   id ref
   (pipmon--plist-to-alist vars)
   (pipmon--pipeline-with-note note)))

(defconst pipmon-buffer-name "*pipmon*")

(defun pipmon-buffer ()
  (get-buffer-create pipmon-buffer-name))

(defun pipmon-log-buffer ()
  (get-buffer-create "*pipmon-log*"))

(defun pipmon-previous-pipeline ()
  (interactive)
  (previous-line)
  (while (not (pipmon--at-pipeline-p))
    (previous-line)))

(defun pipmon-next-pipeline ()
  (interactive)
  (next-line)
  (while (not (pipmon--at-pipeline-p))
    (next-line)))

(defun pipmon-pipeline-toggle ()
  (interactive)
  (when-let (overlay (car (overlays-at (point))))
    (when (pipmon-pipeline-p (overlay-get overlay 'obj))
      (pipmon--toggle-collapsed overlay))))

(defun pipmon-delete-pipeline ()
  (interactive)
  (if-let (overlay (car (overlays-at (point))))
      (let ((pipeline (overlay-get overlay 'obj)))
	(pipmon--delete--pipeline pipeline))))

(defun pipmon-add-pipeline (project-id pipeline-id)
  (interactive "nproject-id: \nnpipeline-id: ")
  (if-let ((overlay (pipmon--find-pipeline-overlay project-id pipeline-id))) 

      ;; Show the pipeline if it already exists.
      (with-current-buffer (pipmon-buffer)
	(goto-char (overlay-start overlay))
	(message "pipeline already exists"))

    ;; Response is handled in a notification.
    (pipmon-rpc-pipeline project-id pipeline-id 'identity)))

(defun pipmon-obj-at ()
  (when-let (overlay (car (overlays-at (point))))
    (overlay-get overlay 'obj)))

(defun pipmon-cancel-pipeline ()
  (interactive)
  (when-let (overlay (car (overlays-at (point))))
    (let ((pipeline (overlay-get overlay 'obj)))
      (when (pipmon-pipeline-p pipeline)
	(pipmon-rpc-cancel-pipeline
	 (pipmon-pipeline-id pipeline)
	 (pipmon-pipeline-pipeline-id pipeline)
	 'identity)))))

(defun pipmon-retry-pipeline ()
  (interactive)
  (when-let (overlay (car (overlays-at (point))))
    (let ((pipeline (overlay-get overlay 'obj)))
      (when (pipmon-pipeline-p pipeline)
	(pipmon--delete--pipeline pipeline)
	(pipmon-rpc-retry-pipeline
	 (pipmon-pipeline-id pipeline)
	 (pipmon-pipeline-pipeline-id pipeline)
	 (pipmon--pipeline-with-note
	  (pipmon-pipeline-note pipeline)))))))

(defun pipmon-retry-job ()
  (interactive)
  (when-let (overlay (car (overlays-at (point))))
    (let ((job (overlay-get overlay 'obj)))
      (when (pipmon-job-p job)
	(-when-let ((pipeline) (pipmon--find-pipeline
				(pipmon-job-id job)
				(pipmon-job-pipeline-id job)))
	  (pipmon--delete--pipeline pipeline)
	  (pipmon-rpc-retry-job
	   (pipmon-job-id job)
	   (pipmon-job-job-id job)
	   (pipmon--pipeline-with-note
	    (pipmon-pipeline-note pipeline))))))))

(defun pipmon-annotate (text)
  (interactive "stext: ")
  (when-let (overlay (car (overlays-at (point))))
    (let ((pipeline (overlay-get overlay 'obj)))
      (when (pipmon-pipeline-p pipeline)
	(setf (pipmon-pipeline-note pipeline) text)
	(pipmon-save)
	(pipmon--redraw overlay)))))

(defun pipmon-log ()
  (interactive)
  (when-let (overlay (pipmon--at-job-p))
    (let* ((job (overlay-get overlay 'obj))
	   (id (pipmon-job-id job))
	   (job-id (pipmon-job-job-id job)))
      (pipmon-rpc-job-trace
       id job-id
       (lambda (result)
	 (with-current-buffer (pipmon-log-buffer)
	   (use-local-map pipmon-log-map)
	   (setq
	    pipmon--running-trace (pipmon-trace--create :consumed 1))
	   (pipmon-rpc-resume-trace)
	   (switch-to-buffer (current-buffer))))))))

(defun pipmon-run (form)
  (interactive (list (read-from-minibuffer "Form: " nil nil t)))
  (eval `,(append form (list `:note (format "%S" `,form)))))

(defun pipmon-rerun ()
  (interactive)
  (pipmon-run (elisp--preceding-sexp)))

(defun pipmon-load ()
  (interactive)
  "Load the database index from the filesystem."
  (let ((index (expand-file-name "index" pipmon--saved-directory))
	(enable-local-variables nil)) ; don't set local variables from index!
    (if (not (file-exists-p index))
	(setf pipmon--pipelines '())
      ;; Override the default value for major-mode. There is no
      ;; preventing find-file-noselect from starting the default major
      ;; mode while also having it handle buffer conversion. Some
      ;; major modes crash Emacs when enabled in large buffers (e.g.
      ;; org-mode).
      (cl-letf (((default-value 'major-mode) 'fundamental-mode))
	(with-current-buffer (find-file-noselect index :nowarn)
	  (goto-char (point-min))
	  (setf pipmon--pipelines (read (current-buffer)))
	  (kill-buffer))))
    (with-current-buffer (pipmon-buffer)
      (erase-buffer)
      (dolist (pipeline (reverse pipmon--pipelines))
	(let ((elt (pipmon--insert-pipeline pipeline)))
	  (dolist (job (reverse (pipmon-pipeline-jobs pipeline)))
	    (pipmon--insert-job job elt))
	  (unless (member (pipmon-pipeline-status pipeline) pipmon--complete-statuses)
	    (pipmon-rpc-pipeline
	     (pipmon-pipeline-id pipeline)
	     (pipmon-pipeline-pipeline-id pipeline)
	     (pipmon--pipeline-with-note
	      (pipmon-pipeline-note pipeline)))))))))

(defun pipmon-save ()
  (interactive)
  (mkdir pipmon--saved-directory t)
  (let ((coding-system-for-write 'utf-8))
    (with-temp-file (expand-file-name "index" pipmon--saved-directory)
      (let ((standard-output (current-buffer))
            (print-level nil)
            (print-length nil)
            (print-circle nil))
	(prin1 pipmon--pipelines)
	:success))))

(defun pipmon--init ()
  (pipmon-rpc--open (pipmon-library-root) pipmon-rpc-python-command)
  (pipmon-load))

;;;###autoload
(defvar pipmon-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)
      (define-key map (kbd "C-c x") #'pipmon-run)
      (define-key map (kbd "C-c X") #'pipmon-rerun)
      (define-key map "p" #'pipmon-previous-pipeline)
      (define-key map "n" #'pipmon-next-pipeline))))

(defvar pipmon-pipeline-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'pipmon-pipeline-toggle)
    (define-key map "d" #'pipmon-delete-pipeline)
    (define-key map (kbd "C-c C") #'pipmon-cancel-pipeline)
    (define-key map (kbd "C-c R") #'pipmon-retry-pipeline)
    map))

(defvar pipmon-job-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")   #'pipmon-log)
    (define-key map (kbd "C-c R") #'pipmon-retry-job)
    map))

(defun pipmon-pause-trace ()
  (interactive)
  (pipmon-rpc-pause-trace))

(defun pipmon-resume-trace ()
  (interactive)
  (pipmon-rpc-resume-trace))

(defun pipmon-quit-trace ()
  (interactive)
  (pipmon-rpc-quit-trace
   (lambda (_) (quit-window t))))

(defvar-keymap pipmon-log-map
  "C-c p" #'pipmon-pause-trace 
  "C-c r" #'pipmon-resume-trace 
  "C-c q" #'pipmon-quit-trace)

(defun pipmon-mode ()
  (interactive)
  ;; (switch-to-buffer (pipmon-buffer))
  (use-local-map pipmon-mode-map)
  (setq major-mode 'pipmon-mode
        mode-name "pipmon"
        truncate-lines t
	buffer-read-only t)
  (buffer-disable-undo)
  (font-lock-mode 1)
  (pipmon--init)
  (run-mode-hooks))

;;;###autoload
(defun pipmon ()
  "Enter pipmon"
  (interactive)
  (unless (eq major-mode 'pipmon-mode)
    (switch-to-buffer (pipmon-buffer))
    (pipmon-mode)))

(provide 'pipmon)
;;; pipmon.el ends here
