(defpackage :google-maps
    (:nicknames :gmaps)
  (:use :parenscript :paren-util :cl)
  ;; globally namespaced
  (:export #:google  
	   #:maps
	   #:-lat-lng
	   #:-map
	   #:set-center
	   #:-map-type-id
	   #:map-type-id
	   #:+roadmap+
	   ;; more lispy
	   #:make-info-window
	   #:make-lat-long
	   #:make-lat-long-bounds
	   #:make-map
	   #:make-map
	   #:make-marker
	   #:ajax-geocode
	   ))

(defpackage :iodb-ui
  (:use :cl :cl-who :css-sexp :parenscript :cl-sails :paren-psos :paren-util
	:google-maps)

  (:export #:output-css
	   #:tab-container-sail
	   #:tab-header-sail
	   #:*browser-info*
	   #:present-modal-sail
	   #:dismiss-modal-sail

	   ;; snazzy forms
	   #:snazzy-form
	   #:snazzy-form-sail
	   #:snazzy-form-sail-view
	   #:snazzy-form-field-sail
	   #:snazzy-form-field
	   #:snazzy-form-field-input
	   #:input-sail
	   #:abstract-input-sail
	   #:textarea-input-sail
	   #:add-form-component
	   #:snazzy-form-component

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

	   ;; google maps
	   #:google-map-sail
	   #:google-map
	   )
  (:shadowing-import-from :parenscript #:attribute))



	   

(in-package :iodb-ui)