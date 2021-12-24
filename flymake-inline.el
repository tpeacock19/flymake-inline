;;; flymake-inline.el --- Display Flymake errors inline -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018 fmdkdd

;; Author: fmdkdd
;; URL: https://github.com/flymake/flymake-inline
;; Keywords: tools, convenience
;; Version: 0.1-cvs
;; Package-Requires: ((emacs "25.1") (flymake "32"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provide an error display function to show Flymake errors inline, directly
;; below their location in the buffer.
;;
;; # Setup
;;
;; Enable the local minor mode for all flymake-mode buffers:
;;
;; (with-eval-after-load 'flymake
;;   (add-hook 'flymake-mode-hook #'flymake-inline-mode))

;;; Code:

(require 'flymake)
(require 'seq)

;;; Displaying line-long overlays (phantoms)

(defun flymake-inline-phantom-display (msg &optional pos err)
  "Display MSG in a phantom directly below POS.

MSG is a string that will be put in a line-long overlay (phantom)
at the line immediately following POS.  If POS is nil, current
point is used instead.

Return the displayed phantom."
  (setq flymake-inline-last-error-point (point))
  (pcase-let* ((p (or pos (point)))
               (e (or err (get-char-property p 'flymake-diagnostic)))
               (`(,pos-bol . ,pos-eol)
                (save-excursion
                  (goto-char p)
                  (while (and (not (= (forward-line) 1))
                              (cl-some (lambda (ov)
                                         (= (point-at-bol)
                                            (overlay-start ov)))
                                       flymake-inline--phantoms)))
                  (cons (point-at-bol) (point-at-eol))))
               (str (concat
                     (when (= pos-eol (point-max))
                       "\n")
                     flymake-inline-prefix
                     msg)))
    ;; don't create duplicate overlays
    (when (not (cl-some (lambda (ov)
                          (member str
                                  (list (overlay-get ov 'after-string)
                                        (overlay-get ov 'display))))
                        flymake-inline--phantoms))
      (let ((ov (make-overlay
                 (if (= pos-eol (point-max))
                     pos-eol
                   pos-bol)
                 pos-eol)))
        (overlay-put ov 'phantom t)
        (overlay-put ov
                     (if (or (= pos-eol (point-max))
                             (= pos-bol pos-eol))
                         'after-string
                       'display)
                     str)
        (overlay-put ov 'error e)
        (push ov flymake-inline--phantoms)
        ov))))

(defun flymake-inline--contains-point (phantom &optional pt)
  "Whether the given error overlay contains the position PT otherwise `(point)'"
  (let* ((pos (or pt (point)))
         (err (overlay-get phantom 'error))
         (region (cons
                  (flymake--diag-orig-beg err)
                  (flymake--diag-orig-end err))))
    (and phantom
         ;; Must be one of our phantoms (probably unneeded).
         (overlay-get phantom 'phantom)
         ;; The underlying error must currently exist.
         err
         (memq err (flymake-diagnostics))
         ;; Most importantly, point must be within the error bounds.
         region
         (>= pos (car region))
         (<= pos (cdr region)))))

(defun flymake-inline-phantom-delete (phantom)
  "Delete PHANTOM if its region doesn't contain point.
Returns the overlay removed or nil."
  (if (flymake-inline--contains-point phantom)
      nil
    (progn (delete-overlay phantom) t)))

(defun flymake-inline-indent-message (offset msg)
  "Indent all lines of MSG by OFFSET spaces.

MSG is trimmed beforehand."
  (let* ((pad (make-string offset ?\s))
         (rep (concat "\n" pad)))
    (concat pad
            (replace-regexp-in-string "\n" rep (string-trim msg)))))


;;; Customization

(defgroup flymake-inline nil
  "Display Flymake errors inline."
  :prefix "flymake-inline-"
  :group 'flymake
  :link '(url-link :tag "Github" "https://github.com/flymake/flymake-inline"))

(defface flymake-inline-error
  '((t :inherit compilation-error))
  "Flymake-inline face for errors."
  :package-version '(flymake-inline . "0.1")
  :group 'flymake-inline)

(defface flymake-inline-warning
  '((t :inherit compilation-warning))
  "Flymake-inline face for warnings."
  :package-version '(flymake-inline . "0.1")
  :group 'flymake-inline)

(defface flymake-inline-note
  '((t :inherit compilation-info))
  "Flymake-inline face for informational messages."
  :package-version '(flymake-inline . "0.1")
  :group 'flymake-inline)

(defcustom flymake-inline-timer-delay 0.5
  "Delay in seconds before displaying errors at point."
  :group 'flymake-inline
  :type 'number
  :safe #'numberp)

(defcustom flymake-inline-prefix "~> "
  "String to be displayed before every error line."
  :group 'flymake-inline
  :type '(choice (const :tag "No prefix" nil)
                 string))

(defvar-local flymake-inline-last-error-point nil)

(defvar-local flymake-inline-timer nil
  "Timer to automatically show the error at point.")

(defcustom flymake-inline-display-function #'flymake-inline-display-phantom
  "Function to display inline errors.

This function is used to display inline all errors at point, as
well as all related errors.  It has the signature (MSG &optional
POS ERR), where MSG is the error message to display, POS its
buffer position, and ERR is the flymake error in general."
  :group 'flymake-inline
  :package-version '(flymake-inline . "0.1")
  :type '(function :tag "Inline error display function")
  :risky t)

(defcustom flymake-inline-clear-function #'flymake-inline-clear-phantoms
  "Function to clear all inline errors.

It takes no arguments and should remove all inline errors created
by `flymake-inline-display-function'."
  :group 'flymake-inline
  :package-version '(flymake-inline . "0.1")
  :type '(function :tag "Inline error clear function")
  :risky t)

(defcustom flymake-inline-display-error-id t
  "Whether to display error IDs inline.

If non-nil, inline errors will contain the error ID.  Error IDs
are optional: not all checkers suplpy this information.  Error
IDs can also be seen in Flymake's error list."
  :group 'flymake-inline
  :type 'boolean
  :package-version '(flymake-inline . "0.1")
  :safe #'booleanp)

;;; Displaying inline errors with phantoms

(defun flymake-inline--displayed-p (err)
  "Whether the given error is displayed with any inline overlays."
  (seq-find (lambda (p) (eq err p))
            flymake-inline--phantoms))

(defvar-local flymake-inline--phantoms nil
  "Remember which phantoms were added to the buffer.")

(defun flymake-inline-display-phantom (msg &optional pos err)
  "Display MSG at POS representing error ERR using phantoms.

POS defaults to point."
  (unless (flymake-inline--displayed-p err)
    (flymake-inline-phantom-display msg pos err)))

(defun flymake-inline-clear-phantoms ()
  "Remove all phantoms from buffer that don't contain point."
  (setq flymake-inline--phantoms (seq-remove #'flymake-inline-phantom-delete flymake-inline--phantoms)) )

(setq flymake-inline--phantoms (cdr flymake-inline--phantoms))

;;; Display inline errors

(defun flymake-inline--error-message (err)
  "Return the message to display for ERR."
  (let ((buffer (flymake-diagnostic-buffer err))
        (id (flymake-diagnostic-type err)))
    (concat (when (and buffer (not (equal buffer (current-buffer))))
              (format "In \"%s\":\n" (buffer-file-name buffer)))
            (flymake-diagnostic-text err)
            (when (and id flymake-inline-display-error-id)
              (format " [%s]" id)))))

(defun flymake-inline--error-face (err)
  "Return the face used to display ERR."
  (pcase (flymake-diagnostic-type err)
    (:note 'flymake-inline-note)
    ('note 'flymake-inline-note)
    ('eglot-note 'flymake-inline-note)
    (:warning 'flymake-inline-warning)
    ('warning 'flymake-inline-warning)
    ('eglot-warning 'flymake-inline-warning)
    (:error 'flymake-inline-error)
    ('error 'flymake-inline-error)
    ('eglot-error 'flymake-inline-error)))

(defun flymake-inline-display-error (err)
  "Display `flymake-error' ERR inline."
  (let* ((pos (flymake-diagnostic-beg err))
         (msg (propertize (flymake-diagnostic-text err)
                          'face (flymake-inline--error-face err))))
    (funcall flymake-inline-display-function msg pos err)))

(defun flymake-inline-hide-errors ()
  "Hide all inline messages currently being shown if point has changed."
  (when (and flymake-inline-last-error-point
             (not (= flymake-inline-last-error-point (point))))
    (funcall flymake-inline-clear-function)))

(defun flymake-inline-display-errors (errors)
  "Display ERRORS, and all related errors, inline.

ERRORS is a list of `flymake-error' objects."
  (flymake-inline-hide-errors)
  (mapc #'flymake-inline-display-error
        (seq-uniq errors)))

(defun flymake-inline-maybe-display ()
  "Display the flymake diagnostic text for the thing at point.
The diagnostic text will be rendered using the function defined
in `flymake-inline-display-diagnostic-function.'"
  (when (and flymake-inline-mode
             (get-char-property (point) 'flymake-diagnostic))
    (let ((errors (get-char-property (point) 'flymake-diagnostic)))
      (funcall #'flymake-inline-display-errors (list errors)))))

(defun flymake-inline-setup ()
  "Setup the hooks for `flymake-inline-mode'."
  (add-hook 'post-command-hook #'flymake-inline-hide-errors nil 'local))

(defun flymake-inline-teardown ()
  "Remove the hooks for `flymake-inline-mode'."
  (remove-hook 'post-command-hook #'flymake-inline-hide-errors 'local)
  (dolist (ov flymake-inline--phantoms)
    (delete-overlay ov))
  (setq flymake-inline--phantoms nil))

;;; Global and local minor modes

;;;###autoload
(define-minor-mode flymake-inline-mode
  "Minor mode for displaying flymake diagnostics at point."
  :lighter nil
  :group flymake-inline
  (cond
   (flymake-inline-mode
    (flymake-inline-setup))
   (t
    (flymake-inline-teardown))))

(defun turn-on-flymake-inline ()
  "Turn on `flymake-inline-mode' in Flymake buffers."
  (if flymake-mode
      (flymake-inline-mode)
    (add-hook 'flymake-mode-hook #'flymake-inline-mode nil 'local)))

(provide 'flymake-inline)

;;; flymake-inline.el ends here
