(defpackage :iodb-ui
  (:use :cl :cl-who :css-sexp :parenscript :cl-sails :paren-psos :paren-util)
  (:export #:output-css
	   #:tab-container-sail
	   #:tab-header-sail
	   #:*browser-info*
	   #:present-modal-sail
	   #:dismiss-modal-sail
	   #:snazzy-form
	   #:snazzy-form-sail
	   #:snazzy-form-sail-view
	   #:snazzy-form-field-sail
	   #:snazzy-form-field
	   #:input-sail
	   #:abstract-input-sail
	   #:textarea-input-sail

	   #:checkbox-input-sail
	   #:add-checkbox
	   #:snazzy-form-field-input

	   #:form-as-json-object
	   #:add-to-json-object
	   #:input-value
	   )
  (:shadowing-import-from :parenscript #:attribute))

(in-package :iodb-ui)