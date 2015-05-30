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

(defmacro test-ob-prolog/test-src-block (number &rest body)
  (declare (indent 1))
  (let ((buf (make-symbol "buf"))
	(visited-p (make-symbol "visited-p"))
	(file-pos (make-symbol "file-pos"))
	(file "test-ob-prolog.org"))
    `(let ((,visited-p (get-file-buffer ,file))
	   (,buf (find-file-noselect ,file)))
       (unwind-protect
	   (with-current-buffer ,buf
	     (goto-char (point-min))
	     (condition-case nil
		 (progn
		   (org-show-subtree)
		   (org-show-block-all))
	       (error nil))
	     (dotimes (,(make-symbol "_") ,number)
	       (org-babel-next-src-block))
	     (save-restriction ,@body))
	 (unless ,visited-p
	   (kill-buffer ,buf))))))
(def-edebug-spec test-ob-prolog/test-src-block (form body))

(ert-deftest test-ob-prolog/simple-execution ()
  "Test simple execution of prolog source block."
  (test-ob-prolog/test-src-block 1
    (should (string= "Hello, org_mode."
		     (org-babel-execute-src-block)))))

(ert-deftest test-ob-prolog/goal-execution ()
  "Test execution of goal argument to prolog source block."
  (test-ob-prolog/test-src-block 2
    (should (string= "Hello, world!"
		     (org-babel-execute-src-block)))))

(ert-deftest test-ob-prolog/simple-running-session ()
  (test-ob-prolog/test-src-block 3
    (should (string= "A = 41."
		     (org-babel-execute-src-block)))))

(ert-deftest test-ob-prolog/simple-running-session ()
  (test-ob-prolog/test-src-block 4
    (should (string= "A = 42."
		     (org-babel-execute-src-block)))))

(ert-deftest test-ob-prolog/simple-running-session ()
  (test-ob-prolog/test-src-block 6
    (should (string= "A = [0, 1, 2, 3]."
		     (org-babel-execute-src-block)))))

;;; test-ob-prolog ends here
