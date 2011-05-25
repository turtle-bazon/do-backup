;;;; -*- mode: lisp -*-

(defsystem :do-backup
  :name "do-backup"
  :author "Azamat S. Kalimoulline <turtle@bazon.ru>"
  :licence "Lessor Lisp General Public License"
  :version "0.0.1.1"
  :description "Backup system"
  :depends-on (external-program
	       external-program-extender)
  :components ((:module "src"
			:components
			((:file "package")
			 (:file "do-backup" :depends-on ("package"))))))
