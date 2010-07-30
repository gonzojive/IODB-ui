(in-package :iodb-ui)

(defparameter *tab-border-color* "#fda966")
(defparameter *tab-selected-background-color* "#fff")
(defparameter *tab-vertical-padding* 4)

(defun output-css (stream)
  "Outputs CSS that is common to the interface elements in the
package.  At the moment this is a bit scrapy considering that cl-sails
has good way of doing CSS already."
  (css-sexp:with-css-output (stream) 
    ;;; autocomplete
    (:div.autocomplete :padding "0" :margin "1px" :z-index "200")
      
    ((ancestor :.autocomplete :ul)
     :background-color "#ffffff"
     :border "1px solid #666666"
     :padding "0"
     :margin "0"
     :cursor "pointer")

    ((ancestor :.autocomplete :li) :list-style-type "none" :display "block" :padding ".2em")

    ((ancestor :.autocomplete :li.selected) :background-color "#0066ff" :color "#dddddd")

    ;;; Tabs
    ((direct-ancestor :.classic-tabs :.tab-header :ul.tabs)
     :text-align "left" ; set to left, right or center
     :margin ".5em 0 .5em 0" ; set margins as desired
     :font-weight "bold"
     :list-style-type "none"
     :padding (raw (format nil "~Apx .4em 3px .5em" (+ 1 *tab-vertical-padding*)))) ; THIRD number must change with respect to padding-top (X) below

    ((direct-ancestor :.classic-tabs :.tab-content)
     :clear "both"
     :border-top (raw (format nil "1px solid ~A" *tab-border-color*))) ;#ffc89c" ;set border COLOR as desired

    ((direct-ancestor :.classic-tabs :.tab-header :ul.tabs :li)
     ;; end
     
     :margin-right "1em"
     :display "block"
     :float "left")

    ((direct-ancestor :.classic-tabs :.tab-header :ul.tabs  :li.selected)
     :border-bottom-width "0px" ; set border color to page background color
;     :background-color (raw *tab-border-color*)
     );; set background color to match above border color

    ((direct-ancestor :.classic-tabs :.tab-header :ul.tabs  :li.selected :a)
     :background-color (raw *tab-selected-background-color*) ;"#fff" ; set selected tab background color as desired
     :color "#000" ; set selected tab link color as desired
     :position "relative"
     :top "1px")

    ((direct-ancestor :.classic-tabs :.tab-header :ul.tabs  :li :a)
     ;; these lines used to be part of :li :a
     :border (raw (format nil "1px solid ~A" *tab-border-color*))   ; set border COLOR as desired; usually matches border color specified in #tabnav
     :-moz-border-radius "3px 3px 0 0"
     :-webkit-border-radius "3px 3px 0 0"
     :padding (raw (format nil "~Apx .7em" (+ 0 *tab-vertical-padding*))) ; set padding (tab size) as desired; FIRST number must change with respect to padding-top (X) above
     :border-bottom "0"

;     :padding "0 .7em"
     :display "block"
     :background-color "#ffe8d7" ; set unselected tab background color as desired
     :color "#666" ; set unselected tab link color as desired
     :margin-right "7px" ; set additional spacing between tabs as desired
     :text-decoration "none"
     :border-bottom "none")

    ((direct-ancestor :.classic-tabs :.tab-header :ul.tabs  :li  :a\:hover)
      :background "white")

    ;; subtabs
    ((direct-ancestor :.subtabs :.tab-header :ul.tabs)
     :text-align "left"
     :margin ".5em 0 0 0"
     :padding "0"
     :list-style-type "none"
     :font-size "90%")

    ((direct-ancestor :.subtabs :.tab-header :ul.tabs :li)
     ;; end
     :margin-right "1em"
     :display "inline")

    ((direct-ancestor :.subtabs :.tab-header :ul.tabs  :li.selected)
     :font-weight "bold")

    ((direct-ancestor :.subtabs :.tab-header :ul.tabs  :li :a)
     :text-decoration "none")

    ((direct-ancestor :.subtabs :.tab-header :ul.tabs  :li :a\:hover)
     :text-decoration "underline")

    ((direct-ancestor :.subtabs :.tab-header :ul.tabs  :li.selected :a)
     :color "#111111"
     :text-decoration "none")

    ((direct-ancestor :.subtabs :.tab-header :ul.tabs  :li.selected :a\:hover)
     :cursor "default"
     :text-decoration "none")
    
    
    ;;;; modal view
    (:.modal-background :position "absolute"
			:top 0
			:background-color "#000000"
			:width "100%"
;			:z-index "99998"
			:height "550px"
			:filter "alpha(opacity=75)"
			:-khtml-opacity "0.75"
			:opacity "0.75"
			)
    
    (:.modal ;:height "500px"
	     :position "absolute"
	     :top "50px"
	     ;:left "50%"
	     ;:margin-left "-500px"
;	     :z-index "99999"
             :width "100%"
	     :filter "alpha(opacity=100)"
	     :-khtml-opacity "1.00"
	     :opacity "1.00"
	     :margin-bottom "2em"
	     ;:background-color "#ffffff"
	     )
    
    ;; snazzy form
    ((or (ancestor :.snazzy-form :p.subtitle)
	 (ancestor :.snazzy-form :h1)
	 (ancestor :.snazzy-form :form)
	 (ancestor :.snazzy-form :.form-field :input))
     :border 0 :margin 0 :padding 0)

    (:.spacer :clear "both")

    (:.snazzy-form :min-height "1px")
    ((ancestor :.snazzy-form :form) :min-height "0" :width "100%")
    ((ancestor :.snazzy-form :.floatholder) :min-height "0" :overflow "auto")

    (:.snazzy-form :padding "14px" :border "solid 2px #b7ddf2" :background-color "#ebf4fb")

    ((ancestor :.snazzy-form :h1) :font-weight "bold" :margin-bottom ".3em")

    ((ancestor :.snazzy-form :p.subtitle)
     :font-size "80%" :color "#666666"  :margin-bottom "2em" :padding-bottom "1em"
     :border-bottom "solid 1px #b7ddf2")
    
    ((ancestor :.snazzy-form :.form-field)
     :overflow "auto"
     :margin "0 0 .7em 0")

    ((ancestor :.snazzy-form :.field-set :h3)
     :border-bottom "1px dotted gray" :margin "0.5em 0em" :padding "0.5em 0.3em")
    
    ((ancestor :.snazzy-form :label.snazzy)
     :font-weight "bold" :text-align "right" :width "10em"
     :float "left"
     :display "inline") ; potential ie6 fix  inline is automatically converted to block by all browsers

    ((ancestor :.snazzy-form :label.snazzy :.note)
     :display "block" :font-weight "normal" :font-size "80%" :text-align "right" :color "#666666")

    ((ancestor :.snazzy-form :.input)
     :margin "2px 0 0 10.5em" :display "block" :padding "0 0 .2em 0")

    ((ancestor :.snazzy-form :.error)
     :margin "5px 0 0 10.5em" :display "block" :padding "0 0 .2em 0"
     :color "#ff0000")

    ((or (ancestor :.snazzy-form  :.form-field :input)
         (ancestor :.snazzy-form  :.form-field :textarea))
     :border "solid 1px #aacfe4")

    ((ancestor :.snazzy-form :.form-field :input)
     ;:width "18em"
     :padding ".15em")
    
    ((ancestor :.snazzy-form :.button-holder)
     :margin-left "10.65em")

    ((ancestor :.snazzy-form :.button-holder :input)
     :margin-right "1.0em")


    ((ancestor :.snazzy-form :.checkbox-item)
     :padding-right ".7em")

    ((ancestor :.snazzy-form :.compound :button)
     :margin "0 0 1px 0")

    ;; Calendar
    (:.cal :margin "10px" :background-color "#C3D9FF" :width "15.41em" :font-size "85%" :border "1px solid #C3D9FF")
 
    ((ancestor :.cal :.calheader) :text-align "center" :padding "0" )
    
    ((ancestor :.cal :.calheader :.title) :overflow "auto" :padding "3px" :font-weight "bold" :color "#0000EE")

    ((ancestor :.cal :.calheader :.prev)  :float "left" :margin-left ".5em")
    
    ((ancestor :.cal :.calheader :.next) :float "right" :margin-right ".5em")

    ((or (ancestor :.cal :ol.calrow) 
	 (ancestor :.cal :ol.headrow))
     :list-style-type "none" :margin 0 :padding 0 :overflow "auto" :display "block")
    
    ((or (ancestor :.cal :ol.calrow :li) 
	 (ancestor :.cal :ol.headrow :li))
     :float "left" :width "1.8em" :padding ".2em .2em":text-align "center")
    
    ((or (ancestor :.cal :ol.calrow :li) )
     :background-color "white")
    
    
    ((or (ancestor :.cal :ol.calrow :li.selected) )
     :background-color "#C3D9FF")
    
    
    ((or (ancestor :.cal :ol.calrow :li\:hover) )
     :background-color "#D3DFFF")
    
    
    ((or (ancestor :.cal :ol.calrow :li.weekend) )
     :background-color "#eeeeee")
    
    ;; Map
    (:.google-map
     :width "400px"
     :height "400px")
    
    ))