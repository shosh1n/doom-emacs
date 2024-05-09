;;; lisp/org-agenda-dashboard.el -*- lexical-binding: t; -*-
;;; lisp/org-agenda-dashboard.el -*- lexical-binding: t; -*-
;;; org-agenda-dashboard.el --- Dashboards for org-agenda   -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021 Nicolas P. Rougier

;; Author: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; Homepage: https://github.com/rougier/mu4e-dashboard
;; Keywords: convenience
;; Version: 0.1.1

;; Package-Requires: ((emacs "26.1"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; mu4e-dashboard provides enhanced org-mode links that allow you to
;; define custom dashboards that link back to the mu4e email client.
;;


(require 'subr-x)
(require 'ob-shell)
(require 'org)

;;; Code:

(defconst org-agenda-dashboard-version "0.0.1")

(defgroup org-agenda-dashboard nil
  "Provides a new Org mode link type for org-agenda-dashboard queries."
  :group 'comm)

(defcustom org-agenda-dashboard-file "~/org/org-agenda-dashboard.org"
  "Path to the dashboard org file."
  :type 'string)

(defcustom org-agenda-dashboard-link-name "orgdash"
  "Default link name."
  :type 'string)

(defcustom org-agenda-dashboard-sidebar-width 40
  "Width of the dashboard sidebar."
  :type 'integer)

(defcustom org-agenda-dashboard-lighter " orgdash"
  "Minor mode lighter indicating that this mode is active."
  :type 'string)

(org-link-set-parameters
 org-agenda-dashboard-link-name
 :follow #'org-agenda-dashboard-follow-agenda-link)

(defvar org-agenda-dashboard--prev-local-keymap nil
  "Buffer-local variable to save the prior keymap.")

(make-variable-buffer-local 'org-agenda-dashboard--prev-local-keymap)

(defvar org-agenda-dashboard--async-update-in-progress nil
  "Set tot t if an async update is in progress.
This is a buffer-local variable that will be t if the current
buffer is in the process of being updated asynchronously.")

(make-variable-buffer-local 'org-agenda-dashboard--async-update-in-progress)

;;;###autoload
(define-minor-mode org-agenda-dashboard-mode
  "Minor mode for \"live\" org-agenda dashboards."
  :lighter org-agenda-dashboard-lighter
  :init-value nil
  (if org-agenda-dashboard-mode
      (progn
        (setq buffer-read-only t)
        ;; Make a copy of the current local keymap (this will, in
        ;; general, have been setup by org-mode, but I don't want to
        ;; assume that)
        (setq org-agenda-dashboard--prev-local-keymap (current-local-map))
	    (use-local-map (make-composed-keymap (org-agenda-dashboard-parse-keymap) (current-local-map)))
	    ;; If buffer corresponds to the dashboard, add a special key (buffer-name is harcoded). Dashboard should be open with a special function naming a defcustom buffer name  and then install the minor mode.
	    ;; install the keymap as local with current map as parent (this might generate some problem?)
	    (if (string= (buffer-file-name) (expand-file-name org-agenda-dashboard-file))
	        (local-set-key (kbd "<return>") #'org-open-at-point))  ; not sure what this is
	    ;; (add-hook 'mu4e-index-updated-hook #'mu4e-dashboard-update)
	    ;; (if org-agenda-dashboard-propagate-keymap
	        ;; install minor mode to mu4e headers view when called (should it be to message hook too?)
	        ;; (add-hook 'mu4e-headers-found-hook #'mu4e-dashboard-mode))
        (org-agenda-dashboard-update))
    (if org-agenda-dashboard--async-update-in-progress
        (user-error "Update in progress; try again when it is complete"))
    ;; (remove-hook 'mu4e-index-updated-hook #'mu4e-dashboard-update)
    ;; clear hook when dashboard disable
    ;; (remove-hook 'mu4e-headers-found-hook #'mu4e-dashboard-mode)
    (use-local-map org-agenda-dashboard--prev-local-keymap)
    (setq buffer-read-only nil)))

(defun org-agenda-dashboard ()
  "If the dashboard file exists, switch to it and run org-agenda-dashboard-mode on it"
  (interactive)
  (if (file-exists-p org-agenda-dashboard-file)
      (progn
        (display-buffer-in-side-window
         (find-file-noselect org-agenda-dashboard-file)
         (list
          (cons 'side 'left)
          (cons 'window-width org-agenda-dashboard-sidebar-width)
          (cons 'window-parameters (list (cons 'no-delete-other-windows t)
                                         (cons 'no-other-window nil)
                                         (cons 'mode-line-format 'none)))))
        (switch-to-buffer-other-window (get-file-buffer org-agenda-dashboard-file))
        (org-agenda-dashboard-mode))
    (message (concat org-agenda-dashboard-file " does not exist"))
    ))

(defun org-agenda-show-matches (query files header)
  (let ((org-agenda-overriding-header (concat header "\n"))
        (org-agenda-files files))
    (progn
      (kill-buffer "*Org Agenda(a)*")
      (org-tags-view nil query)
      (rename-buffer "*Org Agenda(a)*"))))

(defun count-agenda-items  (query files)
  (length (org-map-entries nil query files)))

(defun open-agenda-day-view ()
  (progn
    (other-window 1)
    (kill-buffer "*Org Agenda(a)*")
    (org-agenda nil "a")))

(defun org-agenda-dashboard-follow-agenda-link (path)
  "Process an agenda link with path PATH.
PATH shall be of the form [[mu4e:query|fmt|limit][(---------)]].
PATH shall be of the form [[mu4e:query|file(s)|fmt][(---------)]].
If FMT is not specified or is nil, clicking on the link calls
mu4e with the specified QUERY (with or without the given
LIMIT).  If FMT is specified, the description of the link is
updated with the QUERY count formatted using the provided
format (for example \"%4d\")."

  (let* ((link    (org-element-context))
         (query   (string-trim (nth 0 (split-string path "[]|]"))))
         (files   (nth 1 (split-string path "[]|]")))
         (files   (if (> (length files) 0) (split-string files) org-agenda-files))
         (fmt     (nth 2 (split-string path "[]|]")))
         (description (buffer-substring
                       (org-element-property :contents-begin (org-element-context))
                       (org-element-property :contents-end (org-element-context)))))
    (cond
     ((string-equal query "org-agenda-daily")
      (open-agenda-day-view))
     ((not fmt)
      (other-window 1)
      (org-agenda-show-matches query files description))
     ((and fmt (> (length fmt) 0))
       (org-agenda-dashboard-update-link link)))))

(defun org-agenda-dashboard-update-link (link)
  "Update content of a formatted mu4e LINK.
A formatted link is a link of the form
[[mu4e:query|limit|fmt][(---------)]] where fmt is a non nil
string describing the format.  When a link is cleared, the
description is replaced by a string for the form \"(---)\" and
have the same size as the current description. If the given
format is too big for the current description, description is
replaced with + signs."

  (let* ((path  (org-element-property :path link))
         (query (string-trim (nth 0 (split-string path "|"))))
         (files (nth 1 (split-string path "|")))
         (files (if (> (length files) 0) (split-string files) org-agenda-files))
         (fmt   (nth 2 (split-string path "|")))
         (beg   (org-element-property :contents-begin link))
         (end   (org-element-property :contents-end link))
         (size  (- end beg)))
    (if (and fmt (> (length fmt) 0))
        (let* ((output (count-agenda-items query files))
               (output  (format fmt output)))
          (let ((modified (buffer-modified-p))
                (inhibit-read-only t))
            (save-excursion
              (delete-region beg end)
              (goto-char beg)
              (insert (if (<= (length output) size) output
                        (make-string size ?+))))
            (set-buffer-modified-p modified))))))

;; (defun mu4e-dashboard--async-shell-command-to-string (command callback)
;;   "Run COMMAND asynchronously; call CALLBACK on completion.

;; Run a shell command in an asynchronous way.  Once the call
;; terminates, callback is called with the result."

;;   (let* ((display-buffer-alist (list (cons "\\*Async Shell Command\\*.*"
;;                                        (cons #'display-buffer-no-window nil))))
;;          (output-buffer (generate-new-buffer "*Async Shell Command*"))
;;          (proc (progn
;;                  (async-shell-command command output-buffer)
;;                  (get-buffer-process output-buffer))))
;;     (if (process-live-p proc)
;;         (set-process-sentinel proc
;;                               (lambda (process _signal)
;;                                 (when (memq (process-status process) '(exit signal))
;;                                   (with-current-buffer output-buffer
;;                                     (funcall callback (buffer-string)))
;;                                   (kill-buffer output-buffer))))
;;       (message "No process running."))))


(defun org-agenda-dashboard-update-all-sync ()
  "Update content of all mu4e formatted links in a synchronous way.
A formatted link is a link of the form
[[mu4e:query|limit|fmt][(---------)]] where fmt is a non nil
string describing the format.  When a link is cleared, the
description is replaced by a string for the form \"(---)\" and
have the same size as the current description."

  (org-agenda-dashboard-clear-all)
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (string= (org-element-property :type link) org-agenda-dashboard-link-name)
        (org-agenda-dashboard-update-link link))))
  (redisplay t))

(defun org-agenda-dashboard-clear-link (link)
  "Clear a formatted org-agenda link LINK.
A formatted link is a link of the form
[[mu4e:query|limit|fmt][(---------)]] where fmt is a non nil
string describing the format.  When a link is cleared, the
description is replaced by a string for the form \"(---)\" and
having the same size as the current description."

  (let* ((path (org-element-property :path link))
         (files (nth 1 (split-string path "|")))
         (files (if (> (length files) 0) (split-string files) org-agenda-files))
         (fmt  (nth 2 (split-string path "|")))
         (beg  (org-element-property :contents-begin link))
         (end  (org-element-property :contents-end link))
         (size (- end beg)))
    (if (and fmt (> (length fmt) 0))
        (let ((modified (buffer-modified-p))
              (inhibit-read-only t))
          (save-excursion
            (delete-region beg end)
            (goto-char beg)
            (insert (format "(%s)" (make-string (max (- size 2) 0) ?-))))
          (set-buffer-modified-p modified)))))

(defun org-agenda-dashboard-clear-all ()
  "Clear all formatted mu4e links.
A formatted link is a link of the form
[[mu4e:query|limit|fmt][(---------)]] where fmt is a non nil
string describing the format.  When a link is cleared, the
description is replaced by a string for the form \"(---)\" and
have the same size as the current description."

  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (string= (org-element-property :type link) org-agenda-dashboard-link-name)
        (org-agenda-dashboard-clear-link link))))
  (redisplay t))

(defun org-agenda-dashboard-update ()
  "Update the current dashboard."
  (interactive)
  ;; (message
  ;;  (concat "[" (propertize "org agenda dashboard" 'face 'bold) "] "
  ;;          (format-time-string "Update (%H:%M)")))
  (dolist (buffer (buffer-list (current-buffer)))
    (with-current-buffer buffer
      (if (bound-and-true-p org-agenda-dashboard-mode)
          (org-agenda-dashboard-update-all-sync)))))

(defun org-agenda-dashboard-parse-keymap ()
  "Parse the current buffer file for keybindings.
Keybindings are defined by keywords of type KEYMAP:VALUE and
install the corresponding key bindings in the mu4e-dashboard
minor mode keymap.  The previous keymap (if any) is erased.
VALUE is composed of \"keybinding | function-call\" with
keybinding begin a string describing a key sequence and a call to
an existing function. For example, to have 'q' to kill the
current buffer, the syntax would be:
#+KEYMAP: q | kill-current-buffer
This can be placed anywhere in the org file even though I advised
to group keymaps at the same place."

  (let ((map (make-sparse-keymap)))
    (org-element-map (org-element-parse-buffer) 'keyword
      (lambda (keyword)
	(when (string= (org-element-property :key keyword) "KEYMAP")
          (let* ((value (org-element-property :value keyword))
		 (key   (string-trim (nth 0 (split-string value "|"))))
		 (call  (string-trim (nth 1 (split-string value "|")))))
            (define-key map
	      (kbd key)
	      `(lambda () (interactive) ,(car (read-from-string (format "(%s)" call)))))
            (message "org-agenda-dashboard: binding %s to %s"
		     key
		     (format "(lambda () (interactive) (%s))" call))))))
    map))

(provide 'org-agenda-dashboard)

;;; org-agenda-dashboard.el ends here
