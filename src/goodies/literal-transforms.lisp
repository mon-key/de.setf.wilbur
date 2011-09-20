;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  literal-transforms.lisp
;;;
;;;  This program is licensed under the terms of the GNU Lesser General Public License
;;;  as published by the Free Software Foundation, version 2.1 of the License. Note
;;;  however that a preamble attached below also applies to this program.
;;; :SEE ../src/LICENSE for details
;;;
;;;   Purpose: Interface to load/creation-time literal transformation framework.
;;;


(in-package "WILBUR")


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS LITERAL-TRANSFORM-DB-MIXIN
;;;

(defclass literal-transform-db-mixin ()
  ())

(defmethod db-make-literal ((db literal-transform-db-mixin) string
			    &key language datatype property)
  (multiple-value-bind (string language datatype)
		       (db-transform-literal db string property
					     :language language :datatype datatype)
    (call-next-method db string :language language :datatype datatype)))

(defmethod db-transform-literal ((db literal-transform-db-mixin) string property
				 &key language datatype)
  (declare (ignore property))
  (values string language datatype))
