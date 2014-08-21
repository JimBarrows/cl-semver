(in-package :unit-tests)

(def-suite unit-test-suite :description "All Unit tests")

(in-suite unit-test-suite)

(test version-number-must-be-integer
  (signals version-number-must-be-integer
    (make-instance 'version :major "not integer"))
  (signals version-number-must-be-integer
    (make-instance 'version :minor "not integer"))
  (signals version-number-must-be-integer
    (make-instance 'version :patch "not integer")))

(test version-number-must-be-positive
  (signals version-number-must-be-positive
    (make-instance 'version :major -1))
  (signals version-number-must-be-positive
    (make-instance 'version :minor -1))
  (signals version-number-must-be-positive
    (make-instance 'version :patch -1)))

(test increment-patch-adds-1
  (is (= 1 (patch (increment-patch (make-instance 'version))))))

(test increment-minor-adds-1
  (is (= 2 (minor (increment-minor (make-instance 'version))))))

(test increment-major-adds-1
  (is (= 1 (major (increment-major (make-instance 'version))))))

(test major-version-is-for-initial-development
  (is (initial-development-? (make-instance 'version :major 0)))
  (is (not (initial-development-? (make-instance 'version :major 1)))))

(test patch-version-must-be-reset-to-0-when-minor-version-is-incremented
  (is (= 0 (patch (increment-minor (make-instance 'version :patch 1))))))

(test minor-and-patch-version-must-be-reset-to-0-when-major-version-is-incremented
  (let ((version (increment-major (make-instance 'version :minor 1 :patch 1))))
    (is (= 0 (patch version) (minor version)))))

(test as-string-uses-correct-format
  (is (string= "1.1.1" (as-string (make-instance 'version :major 1 :minor 1 :patch 1))))
  (is (string= "1.1.1-SNAPSHOT" (as-string (make-instance 'version :major 1 :minor 1 :patch 1 :pre-release "SNAPSHOT"))))
  (is (string= "1.1.1+build-info" (as-string (make-instance 'version :major 1 :minor 1 :patch 1 :metadata "build-info"))))
  (is (string= "1.1.1-SNAPSHOT+build-info" (as-string (make-instance 'version :major 1 :minor 1 :patch 1 :pre-release "SNAPSHOT" :metadata "build-info")))))

(test before-works
  (is (before 
       (make-instance 'version :major 1) 
       (make-instance 'version :major 2)))
  (is (not (before 
	    (make-instance 'version :major 3) 
	    (make-instance 'version :major 2))))
  (is (before 
       (make-instance 'version :major 1 :minor 1) 
       (make-instance 'version :major 1 :minor 2)))
  (is (not (before 
	    (make-instance 'version :major 1 :minor 3) 
	    (make-instance 'version :major 1 :minor 2))))
  (is (before 
       (make-instance 'version :major 1 :minor 1 :patch 1) 
       (make-instance 'version :major 1 :minor 1 :patch 2)))
  (is (not (before 
	    (make-instance 'version :major 1 :minor 1 :patch 3) 
	    (make-instance 'version :major 1 :minor 1 :patch 2))))
  (is (before 
       (make-instance 'version :major 1 :minor 1 :patch 1 :pre-release "a") 
       (make-instance 'version :major 1 :minor 1 :patch 1 :pre-release "b")))
  (is (not (before 
	    (make-instance 'version :major 1 :minor 1 :patch 1 :pre-release "c") 
	    (make-instance 'version :major 1 :minor 1 :patch 1 :pre-release "b")))))

(test after-works
  (is (after (make-instance 'version :major 3) (make-instance 'version :major 2)))
  (is (after (make-instance 'version :major 1 :minor 3) (make-instance 'version :major 1 :minor 2)))
  (is (after (make-instance 'version :major 1 :minor 1 :patch 3) (make-instance 'version :major 1 :minor 1 :patch 2)))
  (is (after (make-instance 'version :major 1 :minor 1 :patch 1 :pre-release "c") (make-instance 'version :major 1 :minor 1 :patch 1 :pre-release "b"))))
