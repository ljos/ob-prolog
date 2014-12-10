;;; ob-prolog.el --- org-babel functions for prolog evaluation

;; Copyright (C) Bjarte Johansen

;; Author: Bjarte Johansen
;; Keywords: literate programming, reproducible research
;; Version: 0.0.1

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:


;;; Requirements:

;; - prolog

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
(require 'prolog)

;; The usual file extension for prolog is "pl".
(add-to-list 'org-babel-tangle-lang-exts '("prolog" . "pl"))

(defvar org-babel-default-header-args:prolog
  `((:goal   . nil)
    (:system . ,prolog-system)))

(defun org-babel-execute:prolog (body params)
  "Execute a block of Prolog code with org-babel.  This function is
called by `org-babel-execute-src-block'"
  (message "executing Prolog source code block")
  (let* ((params (org-babel-process-params params))
         (system "swipl")
         (session (cdr (assoc :session params)))
         (goal (cdr (assoc :goal params))))
    (if (string= "none" session)
        (org-babel-prolog-evaluate system goal body)
      (org-babel-prolog-evaluate-session system session goal body))))

(defun org-babel-load-session:prolog (session body params)
  "Load BODY into SESSION."
  (let* ((params (org-babel-process-params params))
         (session (org-babel-prolog-initiate-session
                   (cdr (assoc :system params))
                   (cdr (assoc :session session)))))
    (org-babel-prolog-initiate-session session system)))


(defun org-babel-prolog-evaluate-external-process (system goal body)
  (let* ((tmp-file (org-babel-temp-file "prolog-"))
         (command (concat (format "%s -q -l %s" system tmp-file)
                          (when goal (concat " -t " goal)))))
    (write-region (org-babel-chomp body) nil tmp-file nil 'no-message)
    (with-temp-buffer
      (call-process-shell-command command nil t)
      (buffer-string))))

(defun org-babel-prolog-evaluate-session (system session goal body)
  (let* ((tmp-file (org-babel-temp-file "prolog-"))
         (session (org-babel-prolog-initiate-session system session))
         (prolog-system system)
         (command (prolog-build-prolog-command nil tmp-file tmp-file))
         (body (org-babel-chomp body)))
    (write-region body nil tmp-file nil 'no-message)
    (org-babel-comint-with-output (session "org-babel-prolog-eoe")
      (insert command)
      (comint-send-input)
      (when goal
        (insert (concat goal ",!.\n"))
        (comint-send-input))
      (insert "write('org-babel-prolog-eoe').\n")
      (comint-send-input))
    ;; (with-current-buffer session
    ;;   (let ((process (get-process "prolog")))
    ;;     (with-current-buffer (process-buffer process)
    ;;       (goto-char (process-mark process))
    ;;       (insert command)
    ;;       (process-send-string process command)
    ;;       (when goal
    ;;         (let ((goal (concat goal ",!.\n")))
    ;;           (insert goal)
    ;;           (process-send-string process goal)))
    ;;       (set-marker (process-mark process) (point))
    ;;       (goto-char (process-mark process)))))
    ))

(defun org-babel-prolog-initiate-session (system &optional session)
  "If there is not a current inferior-process-buffer in SESSION
then create.  Return the initialized session."
  (unless (string= session "none")
    (let ((session (get-buffer-create (or session "*prolog*"))))
      (unless (comint-check-proc session)
        (with-current-buffer session
          (prolog-inferior-mode)
          (unless (comint-check-proc session)
            (apply 'make-comint-in-buffer
                   "prolog"
                   (current-buffer)
                   (prolog-program-name)
                   nil
                   (prolog-program-switches)))))
      session)))

(provide 'ob-prolog)
;;; ob-prolog.el ends here
