;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  index-and-match.lisp
;;;
;;;  This program is licensed under the terms of the GNU Lesser General Public License
;;;  as published by the Free Software Foundation, version 2.1 of the License. Note
;;;  however that a preamble attached below also applies to this program.
;;; :SEE ../src/LICENSE for details
;;;
;;;   Purpose: Some additional triple database functionality.
;;;


(in-package "WILBUR")


;;; ----------------------------------------------------------------------------
;;;
;;;   MIXIN CLASS BLANK-NODE-DB-MIXIN
;;;

(defclass blank-node-db-mixin () ; mix before class db
  ((startup-time
    :initform (get-universal-time)
    :reader db-startup-time)
   (blank-node-uri-prefix
    :initarg :blank-node-uri-prefix
    :initform "anon:"
    :reader db-blank-node-uri-prefix)
   (blank-node-index
    :initform 0
    :accessor db-blank-node-index)
   (blank-node->uri
    :initform (make-hash-table :test #'eq)
    :reader db-blank-node->uri)
   (uri->blank-node
    :initform (make-hash-table :test #'equal)
    :reader db-uri->blank-node)))

(defmethod db-resolve-blank-node-uri ((db blank-node-db-mixin) uri)
  (gethash uri (db-uri->blank-node db)))

(defmethod db-blank-node-uri ((db blank-node-db-mixin) (node node) &optional (createp t))
  (let ((node->uri (db-blank-node->uri db)))
    (or (gethash node node->uri)
	(when createp
	  (let ((uri (format nil "~A~X~X"
			     (db-blank-node-uri-prefix db)
			     (incf (db-blank-node-index db))
			     (db-startup-time db))))
	    (setf (gethash uri (db-uri->blank-node db)) node
		  (gethash node node->uri) uri))))))

(defmethod db-blank-node-uri-p ((db blank-node-db-mixin) uri)
  (let ((prefix (db-blank-node-uri-prefix db)))
    (string= uri prefix :end1 (length prefix))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   MIXIN CLASS INDEXED-LITERAL-DB-MIXIN
;;;

(defclass indexed-literal-db-mixin () ; mix before class db
  ((new-literals-lock
    :initform (make-lock)
    :reader db-new-literals-lock)
   (new-literals
    :initform nil
    :accessor db-new-literals)
   (max-string-length
    :initarg :max-string-length
    :initform 4
    :reader db-literal-index-max-string-length)
   (min-string-length
    :initarg :min-string-length
    :initform 2
    :reader db-literal-index-min-string-length)
   (supports-matching-p
    :initform #+(or :cl-ppcre :excl) t #-(or :cl-ppcre :excl) nil
    :reader db-supports-matching-p)
   (index-literals-p
    :initarg :index-literals-p
    :initform nil
    :reader db-index-literals-p)
   (literal-substring-index
    :initform (make-hash-table :test #'equal)
    :reader db-literal-substring-index)))

(defmethod (setf db-literal-index-get) :after ((literal interned-literal)
					       (db indexed-literal-db-mixin) string)
  (declare (ignore string))
  (with-lock ((db-new-literals-lock db))
    (push literal (db-new-literals db))))

(defun db-literal-index-add-substrings (db string literal)
  (let ((min (db-literal-index-min-string-length db))
	(max (db-literal-index-max-string-length db))
	(hash (db-literal-substring-index db)))
    (mapl #'(lambda (s1)
	      (mapl #'(lambda (s2)
			(when (<= min (length s2) max)
			  (pushnew literal
				   (gethash (concatenate 'string (reverse s2)) hash)
				   :test #'literal=)))
		    (reverse s1)))
	  (concatenate 'list string))
    literal))

(defmethod db-find-literals ((db indexed-literal-db-mixin) substring)
  ;; Note: this function matches strings approximately (the substring is broken into
  ;;  segments shorter than the max indexed substring, and segments shorter than the
  ;;  min indexed substring are thrown away. Assumption is that this function is only
  ;;  used to perform initial filtering for an implementation of (say) a regexp match.
  (let ((min (db-literal-index-min-string-length db))
	(max (db-literal-index-max-string-length db))
	(n (length substring))
	(hash (db-literal-substring-index db)))
    (if (<= n max)
      (gethash substring hash)
      (reduce #'(lambda (s1 s2)
		  (intersection s1 s2 :test #'literal=))
	      (mapcar #'(lambda (s)
			  (gethash s hash))
		      (let ((substrings nil))
			(dotimes (i (ceiling n max))
			  (let* ((j (* i max))
				 (k (min (+ j max) n)))
			    (when (>= (- k j) min)
			      (push (subseq substring j k) substrings))))
			substrings))))))

(defmethod db-find-literals-multiple ((db indexed-literal-db-mixin)
				      substring &rest more-substrings)
  (declare (dynamic-extent more-substrings))
  (if more-substrings
    (intersection (apply #'db-find-literals-multiple db more-substrings)
		  (db-find-literals db substring)
		  :test #'literal=)
    (db-find-literals db substring)))

(defmethod db-index-literals ((db indexed-literal-db-mixin))
  (when (db-index-literals-p db)
    (loop (let ((literal (with-lock ((db-new-literals-lock db))
			   (pop (db-new-literals db)))))
	    (if literal
	      (db-literal-index-add-substrings db (literal-string literal) literal)
	      (return-from db-index-literals))))))

(defun convert-match-pattern (pattern)
  (let* ((chars nil)
	 (strings (mapcan #'(lambda (c)
			      (cond ((char= c #\*)
				     (list (concatenate 'string
							(nreverse (shiftf chars nil)))
					   nil))
				    ((= (char-code c) 0)
				     (when chars
				       (list (concatenate 'string (nreverse chars)))))
				    (t
				     (push c chars)
				     nil)))
			  (concatenate 'list pattern (string (code-char 0))))))
    (values (remove-if #'(lambda (s)
			   (or (null s)
			       (< (length s) 2)))
		       strings)
	    (apply #'concatenate 'string (mapcar #'(lambda (s)
						     (or s ".*"))
						 strings)))))

#+(and :cl-ppcre (not :excl))
(defmethod db-match-literals ((db indexed-literal-db-mixin) (pattern string))
  (multiple-value-bind (substrings pattern)
		       (convert-match-pattern pattern)
    (let ((regexp (cl-ppcre:create-scanner pattern)))
      (remove-if-not #'(lambda (literal)
			 (cl-ppcre:all-matches regexp (literal-string literal)))
		     (apply #'db-find-literals-multiple db substrings)))))

#+(and :excl (not :cl-ppcre))
(defmethod db-match-literals ((db indexed-literal-db-mixin) (pattern string))
  (multiple-value-bind (substrings pattern)
		       (convert-match-pattern pattern)
    (let ((regexp (compile-regexp pattern)))
      (remove-if-not #'(lambda (literal)
			 (match-regexp regexp (literal-string literal)))
		     (apply #'db-find-literals-multiple db substrings)))))

#-(or :cl-ppcre :excl)
(defmethod db-match-literals ((db indexed-literal-db-mixin) (pattern string))
  (declare (ignore db pattern))
  nil)
