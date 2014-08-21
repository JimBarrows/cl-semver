(asdf:defsystem #:cl-semver
    :serial t
    :depends-on (#:cl-json
		 #:cl-store
		 #:fiveam)		 
    :components ((:file "package")
		 (:file "semver")
		 (:file "unit-tests")))
