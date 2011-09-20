;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  hash-table.lisp
;;;
;;;  This program is licensed under the terms of the GNU Lesser General Public License
;;;  as published by the Free Software Foundation, version 2.1 of the License. Note
;;;  however that a preamble attached below also applies to this program.
;;; :SEE ../src/LICENSE for details
;;;
;;;   Purpose: A portable interface to hash-tables.
;;;


(in-package "WILBUR")


;;; --------------------------------------------------------------------------------------
;;;
;;;   Some Common Lisp implementations clearly have better hash-table implementations
;;;   than others. I tend to like the MCL/OpenMCL implementation, and I particularly
;;;   dislike the Allegro implementation. In order to allow alternatives to the Common
;;;   Lisp standard hash-tables, we have introduced alternative functions here.
;;;
;;;   If the feature :wilbur-own-hashtables is present we use the implementation in this
;;;   file; this implementation is based on Ingvar Mattson's public domain package
;;;   "genhash" (available from http://www.cliki.net/genhash).
;;;
;;;   the general philosophy here is the same as in the hash-table encapsulation of the
;;;   BEEF frame system (e.g., beef-gethash, etc.).
;;;

;;; --------------------------------------------------------------------------------------
;;;
;;;   
;;;

(defstruct (wilbur-hash-table
	     (:conc-name wht-)
	     (:constructor make-wht (buckets allocated-buckets test)))
  buckets
  allocated-buckets
  (used-buckets 0)
  (stored-items 0)
  (test nil :read-only t))

(defmethod print-object ((self wilbur-hash-table) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~S ~S/~S"
	    (wht-test self)
	    (wht-used-buckets self)
	    (wht-allocated-buckets self))))

(defun expand-hash-table (table)
  (let* ((new-size (1+ (* 2 (wht-allocated-buckets table))))
	 (new-buckets (make-array new-size :initial-element nil))
	 (old-data (wht-buckets table)))
    (setf (wht-allocated-buckets table) new-size
	  (wht-used-buckets table) 0
	  (wht-buckets table) new-buckets)
    (loop for bucket across old-data
	  do (dolist (chunk bucket)
	       (setf (wilbur-gethash (car chunk) table) (cdr chunk))))
    table))


;;; --------------------------------------------------------------------------------------
;;;
;;;   
;;;

(defun wilbur-make-hash-table (&key test
			            (size 17 #-:wilbur-own-hashtables size-provided-p))
  #-:wilbur-own-hashtables
  (if size-provided-p
    (make-hash-table :test test :size size)
    (make-hash-table :test test))
  #+:wilbur-own-hashtables
  (make-wht (make-array size :initial-element nil) size test))

(defun wilbur-gethash (key hash-table &optional default)
  #-:wilbur-own-hashtables
  (gethash key hash-table default)
  #+:wilbur-own-hashtables
  (let ((bucket (svref (wht-buckets hash-table)
		       (mod (sxhash key) (wht-allocated-buckets hash-table))))
	(test (wht-test hash-table)))
    (dolist (chunk bucket (values default nil))
      (when (funcall test (car chunk) key)
	(return (values (cdr chunk) t))))))

(defun (setf wilbur-gethash) (value key hash-table &optional default)
  #-:wilbur-own-hashtables
  (setf (gethash key hash-table default) value)
  #+:wilbur-own-hashtables
  (declare (ignore default))
  #+:wilbur-own-hashtables
  (progn
    (when (= (wht-allocated-buckets hash-table) (wht-used-buckets hash-table))
      (expand-hash-table hash-table))
    (let* ((buckets (wht-buckets hash-table))
	   (size (wht-allocated-buckets hash-table))
	   (bucket-ix (mod (sxhash key) size))
	   (bucket (svref buckets bucket-ix))
	   check)
      (cond ((null (svref buckets bucket-ix))
	     (setf (svref buckets bucket-ix) (cons (cons key value) bucket))
	     (incf (wht-used-buckets hash-table))
	     (incf (wht-stored-items hash-table)))
	    ((setf check (member key bucket :key #'car :test (wht-test hash-table)))
	     (setf (cdr (car check)) value))
	    (t
	     (setf (svref buckets bucket-ix) (cons (cons key value) bucket))
	     (incf (wht-stored-items hash-table))))
      value)))

(defun wilbur-remhash (key hash-table)
  #-:wilbur-own-hashtables
  (remhash key hash-table)
  #+:wilbur-own-hashtables
  (progn
    (when (wilbur-gethash key hash-table nil)
      (let* ((buckets (wht-buckets hash-table))
	     (bucket-ix (mod (sxhash key) (wht-allocated-buckets hash-table)))
	     (bucket (svref buckets bucket-ix)))
	(setf (svref buckets bucket-ix)
	      (delete key bucket :test (wht-test hash-table) :key 'car))
	(unless (svref buckets bucket-ix)
	  (decf (wht-used-buckets hash-table)))
	(decf (wht-stored-items hash-table))))
    t))

(defun wilbur-clrhash (hash-table)
  #-:wilbur-own-hashtables
  (clrhash hash-table)
  #+:wilbur-own-hashtables
  (progn
    (setf (wht-used-buckets hash-table) 0)
    (loop for ix from 0 below (wht-allocated-buckets hash-table)
	  do (setf (svref (wht-buckets hash-table) ix) nil))
    hash-table))

(defun wilbur-hash-table-count (hash-table)
  #-:wilbur-own-hashtables
  (hash-table-count hash-table)
  #+:wilbur-own-hashtables
  (wht-stored-items hash-table))

(defun wilbur-hash-table-size (hash-table)
  #-:wilbur-own-hashtables
  (hash-table-size hash-table)
  #+:wilbur-own-hashtables
  (wht-used-buckets hash-table))

(defun wilbur-maphash (function hash-table)
  #-:wilbur-own-hashtables
  (maphash function hash-table)
  #+:wilbur-own-hashtables
  (let ((buckets (wht-buckets hash-table)))
    (loop for bucket across buckets
	  do (dolist (chunk bucket)
	       (funcall function (car chunk) (cdr chunk))))))

#-:wilbur-own-hashtables
(declaim (inline wilbur-make-hash-table
		 wilbur-gethash
		 (setf wilbur-gethash)
		 wilbur-remhash
		 wilbur-clrhash
		 wilbur-hash-table-count
		 wilbur-hash-table-size
		 wilbur-maphash))
