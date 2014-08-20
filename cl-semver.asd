(asdf:defsystem #:cl-semver
    :serial t
    :depends-on (#:cl-json
		 #:cl-store)		 
    :components ((:file "package")
		 (:file "semver")))
