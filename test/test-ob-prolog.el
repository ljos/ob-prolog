;;; test-ob-prolog.el --- tests for ob-prolog.el

;; Copyright (c) 2015 Bjarte Johansen
;; Authors: Bjarte Johansen

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

;;; Code:

(require 'ert)
(require 'org-id)


(unless (featurep 'ob-prolog)
  (signal 'missing-test-dependency "Support for Prolog code blocks"))

(defmacro test-ob-prolog/test-src-block (name &rest body)
  (declare (indent 1))
  (let ((buf (make-symbol "buf"))
	(visited-p (make-symbol "visited-p"))
	(file-pos (make-symbol "file-pos"))
	(active-session (make-symbol "active-session"))
	(file "test-ob-prolog.org"))
    `(let* ((,visited-p (get-file-buffer ,file))
	    (,buf (or ,visited-p
		      (find-file-noselect ,file)))
	    ,active-session)
       (with-current-buffer ,buf
	 (unwind-protect
	     (save-match-data
	       (save-excursion
		 (goto-char (point-min))
		 (condition-case nil
		     (progn
		       (org-show-subtree)
		       (org-show-block-all))
		   (error nil))
		 (org-babel-goto-named-src-block ,(symbol-name name))
		 (save-excursion
		   (ignore-errors ;; if there is no previous src block.
		     (let* ((info (nth 2 (org-babel-get-src-block-info)))
			    (session (cdr (assq :session info)))
			    (bound (progn
				     (org-babel-previous-src-block)
				     (end-of-line)
				     (point))))
		       (setq ,active-session
			     (unless (string= "none" session)
			       session))
		       (goto-char (point-min))
		       (while (search-forward
			       (concat ":session " session) bound t)
			 (org-babel-execute-src-block)))))
		 (save-restriction ,@body)))
	   (unless ,visited-p
	     (kill-buffer ,buf))
	   (when ,active-session
	     (kill-buffer ,active-session)))))))
(def-edebug-spec test-ob-prolog/test-src-block (form body))

(ert-deftest test-ob-prolog/simple-execution ()
  "Test simple execution of prolog source block."
  (test-ob-prolog/test-src-block basic-test
    (should (string= "Hello, org_mode."
		     (org-babel-execute-src-block)))))

(ert-deftest test-ob-prolog/goal-execution ()
  "Test execution of goal argument to prolog source block."
  (test-ob-prolog/test-src-block goal-test
    (should (string= "Hello, world!"
		     (org-babel-execute-src-block)))))

(ert-deftest test-ob-prolog/simple-running-session ()
  "Test running a session."
  (test-ob-prolog/test-src-block session-test
    (should (string= "A = 41."
		     (org-babel-execute-src-block)))))

(ert-deftest test-ob-prolog/call-predicate-in-session ()
  "Test calling a predicate in the session that is defined in
another block."
  (test-ob-prolog/test-src-block other-predicate-test
    (should (string= "A = 42."
		     (org-babel-execute-src-block)))))

(ert-deftest test-ob-prolog/interacting-with-other-block ()
  "Test interacting with source block that is not a prolog
block."
  (test-ob-prolog/test-src-block interaction-test
    (should (string= "A = [0, 1, 2, 3]."
		     (org-babel-execute-src-block)))))

;;; test-ob-prolog ends here
