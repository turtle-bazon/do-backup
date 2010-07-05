;;;; -*- mode: lisp -*-

(defsystem :do-backup
  :version "0.0.1.0"
  :description "Backup system"
  :depends-on (external-program)
  :components ((:module "src"
			:components
			((:file "package")
			 (:file "utils" :depends-on ("package"))
			 (:file "do-backup" :depends-on ("package"
							 "utils"))))))
