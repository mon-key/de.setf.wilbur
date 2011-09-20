;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  db-additions.lisp
;;;
;;;  This program is licensed under the terms of the GNU Lesser General Public License
;;;  as published by the Free Software Foundation, version 2.1 of the License. Note
;;;  however that a preamble attached below also applies to this program.
;;; :SEE ../src/LICENSE for details
;;;
;;;   Purpose: Some additional triple database functionality
;;;


(in-package "WILBUR")


;;; --------------------------------------------------------------------------------------
;;;
;;;   
;;;

(defmethod db-reify ((triple triple) (db db)
                     &optional (statement-uri nil)
                               (source nil))
  (let ((node (node statement-uri)))
    (flet ((make-and-add-triple (p o)
	     (db-add-triple db (db-make-triple db node (node p) o source))))
      (make-and-add-triple -rdf-subject-uri-   (triple-subject triple))
      (make-and-add-triple -rdf-predicate-uri- (triple-predicate triple))
      (make-and-add-triple -rdf-object-uri-    (triple-object triple))
      (make-and-add-triple -rdf-type-uri-      (node -rdf-statement-uri-))
      node)))

(defmethod is-container-p ((db db) (node node) &optional errorp)
  ;; We may have to extend this to handle subclasses of containers
  (let ((container-types (list (node -rdf-bag-uri-)
                               (node -rdf-seq-uri-)
                               (node -rdf-alt-uri-))))
    (dolist (triple (db-query db node (node -rdf-type-uri-) nil))
      (when (find (triple-object triple) container-types)
        (return-from is-container-p t)))
    (when errorp
      (cerror "Ignore" 'container-required :thing node))))

(defmethod db-find-cbd ((db db) (node node))
  ;; Calculates the Concise Bounded Description as per Patrick Stickler's spec at
  ;; http://www.w3.org/Submission/2004/SUBM-CBD-20040930/
  (cbd (list node) nil nil nil db))

(defun cbd (nodes triples cbd-nodes cbd-triples db)
  (cond (nodes
	 (let ((n (first nodes)))
	   (if (member n cbd-nodes)
	     (cbd (rest nodes) triples cbd-nodes cbd-triples db)
	     (cbd (rest nodes)
		  (append triples (db-query db n nil nil))
		  (cons n cbd-nodes)
		  cbd-triples
		  db))))
	(triples
	 (let ((tr (first triples)))
	   (if (member tr cbd-triples)
	     (cbd nil (rest triples) cbd-nodes cbd-triples db)
	     (cbd (let ((s (triple-reified-p tr db))
			(o (triple-object tr)))
		    (if (and (typep o 'node)
			     (not (typep o 'literal))
			     (null (node-uri o)))
		      (cons o s)
		      s))
		  (rest triples)
		  cbd-nodes
		  (cons tr cbd-triples)
		  db))))
	(t
	 (values cbd-triples cbd-nodes))))

(defmethod db-node-local-properties ((db db) (node node))
  (remove-duplicates (mapcar #'triple-predicate (db-query db node nil nil))))

(defun triple-reified-p (triple db)
  (let ((s-statements (db-query db nil !rdf:subject (triple-subject triple))))
    (when s-statements
      (let ((o-statements (db-query db nil !rdf:object (triple-object triple))))
	(when o-statements
	  (let ((predicate (triple-predicate triple)))
	    (flet ((predicate-not-found (node)
		     (null (db-query db node !rdf:predicate predicate))))
	      (declare (dynamic-extent #'predicate-not-found))
	      (remove-if #'predicate-not-found
			 (intersection (mapcar #'triple-subject s-statements)
				       (mapcar #'triple-subject o-statements))))))))))

(defun get-some-values (frame path db index)
  (assert (null index))
  (db-get-values db frame path))


;;; --------------------------------------------------------------------------------------
;;;
;;;   QUERY EXPRESSION MANIPULATION
;;;

(defun merge-query-expressions (query1 query2)
  (canonical-path `(:or ,(canonical-path query1)
		        ,(canonical-path query2))))


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

(defclass date-cleanup-db-mixin (literal-transform-db-mixin)
  ())

(defmethod db-transform-literal ((db date-cleanup-db-mixin) string (property node)
				 &key language datatype)
  ;; Heuristically transforms time stamps into xsd:date (or xsd:dateTime) literals.
  ;; Only attempt this for dc:date and its recursive sub-properties
  (if (or datatype (not (frames-related-p property !rdfs:subPropertyOf !dc:date db nil)))
    (call-next-method)
    (multiple-value-bind (universal-time omit-time-p)
			 ;; Is it an EXIF-style timestamp?
			 (parse-exif-date string)
      (unless universal-time
	;; Is it an ISO8601 timestamp?
	(multiple-value-setq (universal-time omit-time-p)
	  (ignore-errors (parse-iso8601-date string))))
      (if universal-time
	(values (iso8601-date-string universal-time omit-time-p)
		language
		(if omit-time-p !xsd:date !xsd:dateTime))
	(call-next-method)))))
