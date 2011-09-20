;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  data-sources.lisp
;;;
;;;  This program is licensed under the terms of the GNU Lesser General Public License
;;;  as published by the Free Software Foundation, version 2.1 of the License. Note
;;;  however that a preamble attached below also applies to this program.
;;; :SEE ../src/LICENSE for details
;;;
;;;
;;;   Purpose: Implements the interface to various (loadable) data sources.
;;;


(in-package "WILBUR")


;;; --------------------------------------------------------------------------------------
;;;
;;;   BASIC DATA SOURCE INTERFACE
;;;

(defmethod db-load ((db db) source
		    &rest options
		    &key (error-handling :signal)
		         (merge-results-p (eq error-handling :signal))
		         (clear-temporary-db-p t)
		         (verbosep *load-verbose*)
		         (appendp nil)
		    &allow-other-keys)
  ;; returns: SOURCE-DESC, # TRIPLES, ERRORS
  (declare (dynamic-extent options))
  (remf options :merge-results-p)
  (when verbosep
    (format *standard-output* "~&;Loading RDF: ~S..." (source-locator source))
    (force-output *standard-output*))
  (multiple-value-bind (source-desc temporary-db errors)
		       (apply #'db-load-using-source db source options)
    (when (and source-desc temporary-db merge-results-p)
      (unless appendp
	(db-del-source db (source-desc-url source-desc)))
      (db-merge db temporary-db))
    (multiple-value-prog1 (values source-desc
				  (if (and temporary-db clear-temporary-db-p)
				    (prog1 (length (db-triples temporary-db))
				      (unless (eq db temporary-db)
					(db-clear temporary-db)))
				    temporary-db)
				  errors)
      (when source-desc
	(setf (source-desc-load-annotations source-desc) errors))
      (when verbosep
	(format *error-output* "~:[done~;failed~].~%" errors)))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS SOURCE-DESC
;;;

(defclass source-desc ()
  ((url
    :initarg :url
    :initform nil
    :reader source-desc-url)
   (loaded-from
    :initform nil
    :accessor source-desc-loaded-from
    :initarg :locator
    :reader source-locator)
   (load-time
    :initform nil
    :accessor source-desc-load-time)
   (load-annotations
    :initform nil
    :accessor source-desc-load-annotations)
   (prefix
    :initarg :prefix
    :initform nil
    :accessor source-desc-prefix)))

(defmethod print-object ((self source-desc) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (prin1 (source-desc-url self) stream)))

(defmethod source-locator :around ((self source-desc))
  (or (call-next-method)
      (source-desc-url self)))

(defmethod db-find-source-desc ((db db) (url string) &optional (createp t))
  (db-find-source-desc db (make-url url) createp))

(defmethod db-find-source-desc ((db db) (url url) &optional (createp t))
  (let ((sources (db-source-descs db))
	(url-node (node (url-string url))))
    (or (find url-node sources :key #'source-desc-url)
	(find url-node sources :key #'source-desc-loaded-from)
	(and createp
	     (first (push (make-instance 'source-desc :url url-node)
			  (db-source-descs db)))))))

(defmethod db-find-source-desc ((db db) (url node) &optional (createp t))
  (db-find-source-desc db (node-uri url) createp))

(defmethod db-source-real-url ((db db) (source node))
  (let ((desc (db-find-source-desc db source nil)))
    (and desc (source-desc-loaded-from desc))))

(defmethod db-source-loaded-p ((db db) source)
  (declare (ignore source))
  nil)


;;; --------------------------------------------------------------------------------------
;;;
;;;   DB-LOAD-USING-SOURCE AND FRIENDS
;;;

(defmethod db-load-using-source ((db db) (source string) &rest options)
  (declare (dynamic-extent options))
  (apply #'db-load-using-source db (db-find-source-desc db source) options))

(defmethod db-load-using-source ((db db) (source url)
				 &rest options)
  (declare (dynamic-extent options))
  (apply #'db-load-using-source db (db-find-source-desc db source) options))

(defmethod db-load-using-source ((db db) source ; was: source-desc
				 &rest options
				 &key (locator nil locatorp)
				      (error-handling :signal)
				 &allow-other-keys)
  ;; returns: SOURCE-DESC, TEMPORARY DB, ERRORS
  (declare (dynamic-extent options))
  (assert (typep source 'source-desc))
  (unless (db-find-source-desc db (source-desc-url source) nil)
    (push source (db-source-descs db)))
  (let ((errors nil))
    ;; This mimics the possible expansion of WITH-OPEN-FILE
    (multiple-value-bind (temporary-db source-node)
	(handler-case (let ((abortp t))
			(multiple-value-bind (stream true-url)
					     (source-open-stream source)
			  (remf options :error-handling)
			  (unwind-protect (multiple-value-prog1
					      (apply #'source-fill-db source nil stream
						     (if locatorp
						       locator
						       (url-string true-url))
						     options)
					    (setf abortp nil))
			    (source-close-stream source stream abortp))))
	  (error (e)
	    (ecase error-handling
	      (:signal
	       (cerror "Keep going" e))
	      (:collect
	       (push e errors)
	       (continue e))
	      (:collect-first
	       (push e errors)
	       nil))))
      (when (source-desc-prefix source)
	(add-namespace (source-desc-prefix source)
		       (node-uri (if (source-desc-loaded-from source)
				   (source-desc-url source)
				   source-node))))
      (unless errors
	(setf (source-desc-load-time source) (get-universal-time)
	      (source-desc-loaded-from source) source-node))
      (values source temporary-db errors))))

(defmethod source-fill-db (source db stream locator &rest options)
  (declare (ignore source db))
  (apply #'parse-db-from-stream stream locator options))

(defmethod source-close-stream (source stream &optional abortp)
  (declare (ignore source))
  (close stream :abort abortp))

(defmethod source-locator ((source string)) ; assuming it is a URL
  source)

(defmethod source-locator ((source pathname))
  (namestring source))

(defmethod source-locator ((source logical-pathname))
  (source-locator (translate-logical-pathname source)))

(defmethod source-locator ((source url))
  (url-string source))

(defmethod source-open-stream ((source source-desc))
  (source-open-stream (node-uri (or (source-desc-loaded-from source)
				    (source-desc-url source)))))

(defmethod source-open-stream ((source string))
  (source-open-stream (make-url source)))

#-digitool
(defmethod url-pathname ((url file-url))
  (url-path url))

#+digitool
(defmethod url-pathname ((url file-url))
  (canonical->host-specific-path (url-path url)))
    


(defmethod source-open-stream ((source http-url))
  (multiple-value-bind (response true-url)
		       (http-request source :get)
    (values (http-body response)
	    (or true-url source))))

(defmethod source-open-stream ((source file-url))
  (values (open (url-pathname source) :direction :input)
          source))
  


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS SOURCE-WITH-MODIFICATION
;;;

(defclass source-with-modification ()
  ((original-source
    :accessor source-original-source)
   (original-stream
    :initform nil
    :accessor source-original-stream)))

(defmethod initialize-instance :after ((self source-with-modification)
				       &rest options
				       &key original-source
				       &allow-other-keys)
  (declare (ignore options))
  (setf (source-original-source self) (if (stringp original-source)
					(make-url original-source)
					original-source)))

(defgeneric source-modification (source original-stream))

(defmethod source-locator ((source source-with-modification))
  (source-locator (source-original-source source)))

(defmethod source-open-stream ((source source-with-modification))
  (source-modification source
		       (setf (source-original-stream source)
			     (source-open-stream (source-original-source source)))))

(defmethod source-close-stream :after ((source source-with-modification) stream
				       &optional abortp)
  (declare (ignore stream))
  (source-close-stream (source-original-source source)
		       (shiftf (source-original-stream source) nil)
		       abortp))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS COMPUTED-SOURCE
;;;

(defclass computed-source ()
  ())

(defmethod db-load-using-source ((db db) (source computed-source)
				 &rest options
				 &key (locator (source-locator source))
				 &allow-other-keys)
  (multiple-value-bind (source-desc temp-db errors)
		       (apply #'source-fill-db source db nil locator options)
    (setf (source-desc-load-time source-desc) (get-universal-time)
	  (source-desc-loaded-from source-desc) (source-desc-url source-desc))
    (values source-desc temp-db errors)))

(defmethod source-make-temporary-db ((source computed-source) (db db))
  nil)


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS STRING-SOURCE
;;;

(defclass string-source (computed-source)
  ((locator
    :initarg :locator
    :initform nil
    :reader source-locator)
   (string
    :initarg :string
    :initform nil
    :reader source-string)))

(defmethod source-fill-db ((source string-source) db stream locator &rest options)
  (declare (ignore stream))
  (multiple-value-bind (temporary-db source-node)
		       (with-input-from-string (stream (source-string source))
			 (apply #'parse-db-from-stream stream locator options))
    (values (db-find-source-desc db (make-url (node-uri source-node)))
	    temporary-db
	    nil)))
