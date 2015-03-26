;;; ob-prolog.el --- org-babel functions for prolog evaluation.

;; Copyright (C) Bjarte Johansen

;; Author: Bjarte Johansen
;; Keywords: literate programming, reproducible research
;; URL: https://github.com/ljos/ob-prolog
;; Version: 0.2.0

;; This file is NOT part of GNU Emacs.

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

;; Org-babel support for prolog.
;;
;; To activate ob-prolog add the following to your init.el file:
;;
;;  (eval-after-load 'org
;;    '(require 'org-prolog))
;;

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
(require 'prolog)

(add-to-list 'org-babel-tangle-lang-exts '("prolog" . "pl"))

(defvar org-babel-default-header-args:prolog
  `((:goal   . nil)
    (:system . ,(or prolog-system "swipl"))))

(defun org-babel-prolog--elisp-to-pl (value)
  (cond ((stringp value)
         (format "'%s'"
                 (replace-regexp-in-string
                  "'" "\'" value)))
        ((listp value)
         (concat "[" (mapconcat 'org-babel-prolog--elisp-to-pl value ", ") "]"))
        (t (prin1-to-string value))))

(defun org-babel-prolog--variable-assignment (pair)
  (format "recorda('%s', %s)"
          (car pair)
          (org-babel-prolog--elisp-to-pl
           (cdr pair))))

(defun org-babel-variable-assignments:prolog (params)
  (let ((strs (mapcar #'org-babel-prolog--variable-assignment
                      (mapcar #'cdr
                              (org-babel-get-header params :var)))))
    (when strs
      (list
       (concat ":- " (mapconcat #'identity strs ", ") ".\n")))))

(defun org-babel-prolog--parse-goal (goal)
  "Evaluate inline emacs-lisp in prolog goal parameter.

Example:
      append(=(+ 2 3), =(quote a), B)
   => append(5, a, B)"
  (with-temp-buffer
    (insert goal)
    (while (search-backward "=" nil t)
      (delete-char 1 t)
      (forward-sexp)
      (let ((value (eval (preceding-sexp))))
        (kill-sexp -1)
        (insert (format "%S" value))))
    (buffer-string)))

(defun org-babel-execute:prolog (body params)
  "Execute a block of Prolog code with org-babel.  This function is
called by `org-babel-execute-src-block'"
  (message "executing Prolog source code block")
  (let* ((params (org-babel-process-params params))
         (system (cdr (assoc :system params)))
         (session (cdr (assoc :session params)))
         (goal (org-babel-prolog--parse-goal
                (cdr (assoc :goal params))))
         (vars (org-babel-variable-assignments:prolog params))
         (full-body (org-babel-expand-body:generic body params vars)))
    (if (string= "none" session)
        (org-babel-prolog-evaluate-external-process system goal full-body)
      (org-babel-prolog-evaluate-session system session goal full-body))))

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
  "Evaluates the GOAL in the BODY of the prolog block in the
given SESSION with SYSTEM. If there is no SESSION it creates it."
  (let* ((session (org-babel-prolog-initiate-session system session))
         (body (split-string (org-babel-trim body) "\n")))
    (org-babel-trim
     (with-temp-buffer
       (with-current-buffer session
         (setq comint-prompt-regexp "^|: *"))
       (org-babel-comint-input-command session "consult(user).\n")
       (apply #'insert
              (org-babel-comint-with-output (session "\n")
                (setq comint-prompt-regexp (prolog-prompt-regexp))
                (dolist (line body)
                  (insert line)
                  (comint-send-input nil t)
                  (accept-process-output
                   (get-buffer-process session)))
                (comint-send-eof)))
       (ansi-color-apply-on-region (point-min) (point-max))
       (goto-char (point-max))
       (if (save-excursion
             (search-backward "ERROR: " nil t))
           (progn
             (save-excursion
               (while (search-backward "|: " nil t)
                 (replace-match "" nil t)))
             (search-backward "true." nil t)
             (kill-whole-line)
             (org-babel-eval-error-notify -1 (buffer-string))
             (buffer-string))
         (when goal
           (kill-region (point-min) (point-max))
           (apply #'insert
                  (org-babel-comint-with-output (session "")
                    (insert (concat goal ", !."))
                    (comint-send-input nil t))))
         (ansi-color-apply-on-region (point-min) (point-max))
         (if (not (save-excursion
                    (search-backward "ERROR: " nil t)))
             (let ((delete-trailing-lines t))
               (delete-trailing-whitespace (point-min))
               (buffer-string))
           (search-backward "?-" nil t)
           (kill-whole-line)
           (org-babel-eval-error-notify -1 (buffer-string))
           (buffer-string)))))))

(defun org-babel-prolog--answer-correction (string)
  (when (string-match-p "Correct to: \".*\"\\?" string)
    (insert "no")
    (comint-send-input nil t)))

(defun org-babel-prolog--exit-debug (string)
  (when (string-match-p "\\(.\\|\n\\)*Exception.* \\? $" string)
    (insert "no debug")
    (comint-send-input nil t)))

(defun org-babel-prolog-initiate-session (system &optional session)
  "If there is not a current inferior-process-buffer in SESSION
then create.  Return the initialized session."
  (unless (string= session "none")
    (let ((session (get-buffer-create (or session "*prolog*"))))
      (unless (comint-check-proc session)
        (with-current-buffer session
          (kill-region (point-min) (point-max))
          (prolog-inferior-mode)
          (setq prolog-program-name system)
          (apply 'make-comint-in-buffer
                 "prolog"
                 (current-buffer)
                 (prolog-program-name)
                 nil
                 (cons "-q" (prolog-program-switches)))
          (add-hook 'comint-output-filter-functions
                    'org-babel-prolog--answer-correction
                    nil t)
          (add-hook 'comint-output-filter-functions
                    'org-babel-prolog--exit-debug
                    nil t)
          (while (progn
                   (goto-char comint-last-input-end)
                   (not (save-excursion
                          (re-search-forward comint-prompt-regexp nil t))))
            (accept-process-output
             (get-buffer-process session)))))
      session)))

(provide 'ob-prolog)
;;; ob-prolog.el ends here
