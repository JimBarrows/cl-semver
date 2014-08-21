(in-package :cl-user)

(defpackage cl-semver
  (:use :cl :cl-store)
  (:export version 
	   major
	   minor
	   patch
	   pre-release
	   metadata
	   initial-development-?
	   increment-patch
	   increment-minor
	   increment-major
	   as-string
	   before
	   after
	   version-number-must-be-integer
	   version-number-must-be-positive))


(defpackage unit-tests
  (:use :cl :cl-semver :it.bese.fiveam ))
