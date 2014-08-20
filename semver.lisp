(in-package :cl-semver)

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
		:reader pre-release)
   (metadata :initarg :metadata
	     :reader metadata)))

(defmethod increment-patch((version version))
  (make-instance 'version :major (major version)
		 :minor (minor version)
		 :patch (+ 1 (patch version))
		 :snapshot (snapshot version)))

(defmethod increment-minor((version version))
  (make-instance 'version :major (major version)
		 :minor (+ 1 (minor version))
		 :patch 0
		 :snapshot (snapshot version)))

(defmethod increment-major((version version))
  (make-instance 'version :major (+ 1 (major version))
		 :minor 0
		 :patch 0
		 :snapshot (snapshot version)))

(defmethod as-string((version version))
  (format nil "~d.~d.~d~@[-~a~]~@[+~a~]" (major version)
	  (minor version)
	  (patch version)
	  (pre-release version)
	  (metadata version)))

(defmethod < ((this version)(that version))
  (or (< (major this) (major that))
      (and (= (major this) (major that))
	   (< (minor this) (minor that)))
      (and (= (major this) (major that))
	   (= (minor this) (minor that))
	   (< (patch this) (patch that)))))

(defmethod > ((this version)(that version))
  (or (> (major this) (major that))
      (and (= (major this) (major that))
	   (> (minor this) (minor that)))
      (and (= (major this) (major that))
	   (= (minor this) (minor that))
	   (> (patch this) (patch that)))))


(defmethod save-version((version version))
  (cl-store:store version "version"))

(defun load-version()
  (when (probe-file "version")
    (cl-store:restore "version")))
