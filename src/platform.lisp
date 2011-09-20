;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  platform.lisp
;;;
;;;  This program is licensed under the terms of the GNU Lesser General Public License
;;;  as published by the Free Software Foundation, version 2.1 of the License. Note
;;;  however that a preamble attached below also applies to this program.
;;; :SEE ../src/LICENSE for details
;;;
;;;   Purpose: This file contains various platform-dependent functions and macros.
;;;   Currently, we support MCL, OpenMCL, Allegro and SBCL. There is no reason why Wilbur
;;;   wouldn't run on other Common Lisps too, but some of these functions will have to be
;;;   ported separately.
;;;


(in-package "WILBUR")


;;; --------------------------------------------------------------------------------------
;;;
;;;   FEATURES, PACKAGES, ETC.
;;;

#+digitool
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :realmcl *features*)
  (require :opentransport))

;; #+(or :excl :sbcl)
#+excl ;; 2010-08-03 aserve failed to build with sbcl 1.0.36
(eval-when (:load-toplevel :compile-toplevel :execute)
  ;; adding this feature suppresses other HTTP client implementations
  (pushnew :http-using-aserve *features*)
  ;; other implementations may have other means of installing Portable AServe
  (require :aserve))

#+:excl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(mp:process-kill mp:process-wait mp:process-run-function)))

#+:sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(sb-ext:process-wait)))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (pushnew :wilbur *features*)
  (pushnew :wilbur2 *features*))


;;; --------------------------------------------------------------------------------------
;;;
;;;   LOCKS
;;;

#-(and :digitool :CCL-5.2)              ; already defined
(defmacro with-lock ((lock &rest args) &body body)
  #+(or :digitool :clozure)  `(with-lock-grabbed (,lock ,@args) ,@body)
  #+:excl `(mp:with-process-lock (,lock ,@args) ,@body)
  #+:sbcl `(sb-thread:with-recursive-lock (,lock ,@args) ,@body)
  #-(or :digitool :clozure :excl :sbcl :lispworks) (error "No locking defined (WITH-LOCK)"))

#-(or :digitool :lispworks :clozure) ; unless already implemented
(defun make-lock ()
  #+:excl            (mp:make-process-lock)
  #+:sbcl            (sb-thread:make-mutex)
  #-(or :excl :sbcl) (error "No locking implemented"))


;;; --------------------------------------------------------------------------------------
;;;
;;;   EXTERNAL PROCESSES, ETC.
;;;

(defun simple-external-process (cmd &rest args)
  (declare (dynamic-extent args))
  #+:openmcl (external-process-output-stream
	      (run-program cmd (remove nil args) :output :stream :wait nil))
  #+:excl    (run-shell-command (format nil "~A~{~@[ '~A'~]~}" cmd args)
				:output :stream :wait nil)
  #+:sbcl    (sb-ext:process-output
	      (sb-ext:run-program cmd (remove nil args) :output :stream :wait nil))
  #-(or :openmcl :excl :sbcl)
  (error "Cannot execute \"~A~{~@[ ~A~]~}\". No external processes" cmd args))

(defun quit-lisp-process ()
  #+(or :digitool :openmcl)   (ccl:quit)
  #+:excl                     (excl:exit)
  #+:sbcl                     (sb-ext:quit)
  #-(or :openmcl :excl :digitool :sbcl)
  (warn "Don't know how to quit Lisp"))

(defun get-env (key)
  #+:openmcl                  (ccl:getenv key)
  #+:excl                     (sys:getenv key)
  #+:digitool                 (bsd:getenv key)
  #+:sbcl                     (sb-ext:posix-getenv (string key))
  #-(or :openmcl :excl :digitool :sbcl)
  (error "Cannot get the environment variable ~S" key))
