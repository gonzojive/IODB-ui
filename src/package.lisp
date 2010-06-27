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
	:paren-events :google-maps)
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

           ;; hinting input box
           #:make-input-hint
	   
	   ;; snazzy form
	   #:snazzy-form
	   #:snazzy-form-sail
	   #:snazzy-form-sail-view
	   #:snazzy-form-field-sail
	   #:snazzy-form-field
	   #:snazzy-form-field-input
	   #:snazzy-form-form
           #:snazzy-form-input-value
	   #:input-sail
	   #:abstract-input-sail
	   #:compound-input-sail
	   #:remove-input-event
	   #:add-input-event
	   #:textarea-input-sail
	   #:add-form-component
	   #:snazzy-form-component
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

	   ;; google maps
	   #:google-map-sail
	   #:google-map
	   )
  (:shadowing-import-from :parenscript #:attribute))



	   

(in-package :iodb-ui)