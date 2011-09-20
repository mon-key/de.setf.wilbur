;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  ivanhoe.lisp
;;;
;;;  This program is licensed under the terms of the GNU Lesser General Public License
;;;  as published by the Free Software Foundation, version 2.1 of the License. Note
;;;  however that a preamble attached below also applies to this program.
;;; :SEE ../src/LICENSE for details
;;;
;;;   Purpose: The old "db-hiding" frame API ("Ivanhoe") is grandfathered here.
;;;


(in-package "WILBUR")


;;; --------------------------------------------------------------------------------------
;;;
;;;   OLD "TOP-LEVEL" API
;;;

(defvar *db* nil) ; "current" database

(defun triple (subject predicate object &optional source)
  (db-make-triple *db* subject predicate object source))

(defun add-triple (triple)
  (db-add-triple *db* triple))

(defun del-triple (triple)
  (db-del-triple *db* triple))

(defun query (subject predicate object)
  (db-query *db* subject predicate object))

(defun reify (triple &key (statement-uri nil) (source nil))
  (db-reify triple *db* statement-uri source))


;;; --------------------------------------------------------------------------------------
;;;
;;;   FRAME SYSTEM API IMPLEMENTATION
;;;

(defun frame (uri &rest slot/value-pairs)
  (let ((frame (node uri)))
    (dolist (slot/value-pair slot/value-pairs)
      (destructuring-bind (slot . value) slot/value-pair
        (add-value frame slot value)))
    frame))

(defun own-slots (frame)
  (remove-duplicates (mapcar #'triple-predicate (db-query *db* frame nil nil))))

(defun value (frame &rest paths)
  (declare (dynamic-extent paths))
  (flet ((v (path) (get-value frame path *db*)))
    (declare (dynamic-extent #'v))
  (apply #'values (mapcar #'v paths))))

(defun all-values (frame &rest paths)
  (declare (dynamic-extent paths))
  (flet ((av (path) (get-all-values frame path *db*)))
    (declare (dynamic-extent #'av))
    (apply #'values (mapcar #'av paths))))

(defun add-value (frame path value)
  (db-add-triple *db* (db-make-triple *db* frame path value))
  value)

(defun del-value (frame path &optional value)
  (dolist (triple (db-query *db* frame path value))
    (db-del-triple *db* triple)))

(defun relatedp (source path sink &optional action)
  (frames-related-p source path sink *db* action))

(defun load-db (source &rest options)
  (declare (dynamic-extent options))
  (apply #'db-load *db* source options))
