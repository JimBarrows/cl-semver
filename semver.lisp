(in-package :cl-semver)

(define-condition version-number-must-be-positive (error)())
(define-condition version-number-must-be-integer (error)())

(defclass version()
  ((major :initarg :major
	  :initform 0
	  :reader major)
   (minor :initarg :minor
	  :initform 1
	  :reader minor)
   (patch :initarg :patch
	  :initform 0
	  :reader patch)
   (pre-release :initarg :pre-release
		:initform nil
		:reader pre-release)
   (metadata :initarg :metadata
	     :initform nil
	     :reader metadata)))

(defmethod initialize-instance :after ((version version) &key)
  (unless (and (typep (major version) 'integer)
	       (typep (minor version) 'integer)
	       (typep (patch version) 'integer))
    (signal 'version-number-must-be-integer))
  (unless (and (>= (major version) 0)
	       (>= (minor version) 0)
	       (>= (patch version) 0))
    (signal 'version-number-must-be-positive)))

(defmethod initial-development-? ((version version))
  (= 0 (major version)))

(defmethod increment-patch((version version))
  (make-instance 'version :major (major version)
		 :minor (minor version)
		 :patch (+ 1 (patch version))
		 :pre-release (pre-release version)
		 :metadata (metadata version) ))

(defmethod increment-minor((version version))
  (make-instance 'version :major (major version)
		 :minor (+ 1 (minor version))
		 :patch 0
		 :pre-release (pre-release version)
		 :metadata (metadata version)))

(defmethod increment-major((version version))
  (make-instance 'version :major (+ 1 (major version))
		 :minor 0
		 :patch 0
		 :pre-release (pre-release version)
		 :metadata (metadata version)))

(defmethod as-string((version version))
  (format nil "~d.~d.~d~@[-~a~]~@[+~a~]" (major version)
	  (minor version)
	  (patch version)
	  (pre-release version)
	  (metadata version)))

(defmethod before ((this version)(that version))
  (or (< (major this) (major that))
      (and (= (major this) (major that))
	   (< (minor this) (minor that)))
      (and (= (major this) (major that))
	   (= (minor this) (minor that))
	   (< (patch this) (patch that)))
      (and (= (major this) (major that))
	   (= (minor this) (minor that))
	   (= (patch this) (patch that))
	   (string< (pre-release this) (pre-release that)))))

(defmethod after ((this version)(that version))
  (or (> (major this) (major that))
      (and (= (major this) (major that))
	   (> (minor this) (minor that)))
      (and (= (major this) (major that))
	   (= (minor this) (minor that))
	   (> (patch this) (patch that)))
      (and (= (major this) (major that))
	   (= (minor this) (minor that))
	   (= (patch this) (patch that))
	   (string> (pre-release this) (pre-release that)))))


(defmethod save-version((version version))
  (cl-store:store version "version"))

(defun load-version()
  (when (probe-file "version")
    (cl-store:restore "version")))
