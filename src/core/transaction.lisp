;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  transaction.lisp
;;;
;;;  This program is licensed under the terms of the GNU Lesser General Public License
;;;  as published by the Free Software Foundation, version 2.1 of the License. Note
;;;  however that a preamble attached below also applies to this program.
;;; :SEE ../src/LICENSE for details
;;;
;;;   Purpose: Transaction functionality for triple store databases
;;;


(in-package "WILBUR")


;;; --------------------------------------------------------------------------------------
;;;
;;;   
;;;

;; :NOTE transaction-lock is mutex as per `sb-thread:make-mutex'
;; with-triple-lock -> sb-thread:with-recursive-lock
(defclass transaction-db-mixin ()
  ((transaction-lock
    :initform (make-lock)
    :reader db-transaction-lock)
   (transaction-level
    :initform 0
    :accessor db-transaction-level)))

(defmacro with-transaction-lock (db &body body)
  `(with-lock ((db-transaction-lock ,db)) ,@body))

(defmacro with-transaction (db &key form commit rollback)
  (with-temps (d abortp)
    `(let ((,d ,db))
       (with-transaction-lock ,d
	 (unwind-protect (let ((,abortp t))
			   (incf (db-transaction-level ,d))
			   (prog1 ,form
			     (setf ,abortp nil)))
	   (when (zerop (decf (db-transaction-level ,d)))
	     (if ,abortp ,rollback ,commit)))))))

(defmethod db-add-triple :around ((db locked-db-mixin) (triple triple))
  (with-triple-lock db
    (call-next-method)))

(defmethod db-del-triple :around ((db locked-db-mixin) (triple triple) &optional source)
  (declare (ignore source))
  (with-triple-lock db
    (call-next-method)))

(defmethod db-merge :around ((to locked-db-mixin) (from db) &optional source)
  (declare (ignore source))
  (with-triple-lock to
    (call-next-method)))
