(defpackage :iodb-ui
  (:use :cl :cl-who :css-sexp :parenscript :cl-sails :paren-psos :paren-util :paren-events)
  (:export #:output-css

	   ;; tabs
	   #:tab-container-sail
	   #:tab-header-sail
	   #:tab-sail
	   #:tab-content
	   #:tab-header
	   #:add-tab
	   #:select-tab
	   #:remove-tab
	   #:autocomplete-input
	   #:*browser-info*

	   ;; modal sails
	   #:present-modal-sail
	   #:dismiss-modal-sail
	   
	   ;; snazzy form
	   #:snazzy-form
	   #:snazzy-form-sail
	   #:snazzy-form-sail-view
	   #:snazzy-form-field-sail
	   #:snazzy-form-field
	   #:snazzy-form-field-input
	   #:input-sail
	   #:abstract-input-sail
	   #:textarea-input-sail
	   #:add-form-field

	   #:select-input-sail
	   #:add-option

	   #:checkbox-input-sail
	   #:add-checkbox
	   #:radio-input-sail
	   #:add-radio
	   #:snazzy-form-field-input

	   #:form-as-json-object
	   #:add-to-json-object
	   #:input-value
	   #:as-json-value
	   #:json-value
	   #:set-input-value
	   #:dom-input
	   )
  (:shadowing-import-from :parenscript #:attribute))

(in-package :iodb-ui)