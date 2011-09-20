;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  useful.lisp
;;;
;;;  This program is licensed under the terms of the GNU Lesser General Public License
;;;  as published by the Free Software Foundation, version 2.1 of the License. Note
;;;  however that a preamble attached below also applies to this program.
;;; :SEE ../src/LICENSE for details
;;;
;;;   Purpose: Useful functions and macros
;;;


(in-package "WILBUR")


;;; --------------------------------------------------------------------------------------
;;;
;;;   GENERALLY USEFUL STUFF
;;;

(defmacro with-temps ((&rest variables) &body body)
  `(let (,@(mapcar #'(lambda (variable)
		       `(,variable (gentemp)))
		   variables))
    ,@body))

(defmacro dolist+ ((pattern list &optional (value nil value-supplied-p)) &body body)
  (if (symbolp pattern)
    `(dolist (,pattern ,list ,@(and value-supplied-p (list value)))
       ,@body)
    (let ((i (gentemp)))
      `(dolist (,i ,list ,@(and value-supplied-p (list value)))
	 (destructuring-bind ,pattern ,i
	   ,@body)))))

(defmacro dsb (pattern form &body body)
  `(destructuring-bind ,pattern ,form ,@body))

(defun remove-weird (sequence item &rest options)
  (declare (dynamic-extent options))
  (apply #'remove item sequence options))

(defun delete-weird (sequence item &rest options)
  (declare (dynamic-extent options))
  (apply #'delete item sequence options))

(define-modify-macro removef (items &rest options) remove-weird)

(define-modify-macro deletef (items &rest options) delete-weird)

(define-modify-macro unionf (items) union)

(defun eq~ (x y)
  (or (null x)
      (null y)
      (eq x y)))

(declaim (inline eq~))

(defun string->keyword (string &optional (package :keyword))
  (if package (intern (string-upcase string) package) string))


(defmacro defequal (name value &optional documentation)
  (let ((name-var (gensym)))
    `(defconstant ,name
       (let ((,name-var ,value))
         (if (boundp ',name)
           (progn (assert (equalp (symbol-value ',name) ,name-var) ()
                          "Previous value for constant ~a not equal to new binding: ~s."
                          ',name ,name-var)
                  (symbol-value ',name))
           ,name-var))
       ,@(when documentation (list documentation)))))

;;; --------------------------------------------------------------------------------------
;;;
;;;   STRING DICTIONARY
;;;
;;;   Some care must be taken when using this, since (in the interest of making the
;;;   implementation not cons so much) we have used destructive operations.
;;;

(defun string-dict-get (keys&values key)
  (cdr (assoc key keys&values :test #'string=)))

(defun string-dict-get-by-value (keys&values value)
  (car (rassoc value keys&values :test #'string=)))

(defun string-dict-add (keys&values key value)
  (acons key value keys&values))

(defun string-dict-del (keys&values key)
  (delete key keys&values :key #'car :test #'string=))

(defmacro do-string-dict ((key value dict) &body body)
  `(loop for (,key . ,value) in ,dict do (progn ,@body)))


;;; --------------------------------------------------------------------------------------
;;;
;;;   LIST MANIPULATION
;;;

(defun split-list (head tail n &optional (no-split-p nil))
  (if no-split-p
    (values tail nil)
    (if (and tail (plusp n))
      (split-list (cons (first tail) head) (rest tail) (1- n) no-split-p)
      (values (nreverse head) tail))))

(defun prioritize-list (list possible-priority-items
			&key (test #'eql) (key #'identity))
  (prioritize list :prefer possible-priority-items :test test :key key))

(defun prioritize (list
		   &key (prefer nil)
		        (exclude nil)
		        (test #'eql)
		        (key #'identity)
		        (splitp nil))
  (let* ((items (remove-if #'(lambda (item)
			       (find-if #'(lambda (e)
					    (funcall test e (funcall key item)))
					exclude))
			   list))
	 (priority-items (mapcan #'(lambda (p)
				     (let ((item (find p items :test test :key key)))
				       (and item (list item))))
				 prefer))
	 (other-items (remove-if #'(lambda (item)
				     (find-if #'(lambda (p)
						  (funcall test
							   (funcall key p)
							   (funcall key item)))
					      priority-items))
				 items)))
    (if splitp
      (values priority-items other-items)
      (append priority-items other-items))))
