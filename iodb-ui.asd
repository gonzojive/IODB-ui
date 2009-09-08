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
			   (:parenscript-file "modal-sail" :depends-on ("browserdetect"))
			   (:parenscript-file "snazzy-form" :depends-on ("browserdetect"))
			   #+nil
			   (:parenscript-file "calendar-picker" :depends-on ("browserdetect"))
			   #+nil
			   (:parenscript-file "datetime-range-picker" :depends-on ("calendar-picker" "autocomplete" "browserdetect"))
			   ))
			 )))

  :depends-on ("cl-who" "css-sexp" "cl-sails"))
