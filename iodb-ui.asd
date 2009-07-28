(defpackage :iodb-ui.script
  (:use #:cl #:asdf))

(in-package :iodb-ui.script)

(asdf:operate 'asdf:load-op :paren-files)

(defsystem :iodb-ui
  :description ""
  :version "0.0.1"
  :author "Red Daly"
  :license "No one is licensed to use this, not even myself."
  :components ((:module "src"
			:components
			((:file "package")
			 (:file "css" :depends-on ("package"))
			 (:module "paren"
			  :components
			  ((:parenscript-file "tabs")
			   (:parenscript-file "browserdetect")
			   (:parenscript-file "autocomplete" :depends-on ("browserdetect"))
			   ))
			 
			 )))

  :depends-on ("cl-who" "css-sexp" "cl-sails"))
