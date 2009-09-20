(in-package :iodb-ui)

(defun output-css (stream)
  (css-sexp:with-css-output (stream) 
    ;;; autocomplete
    (:div.autocomplete :padding "0" :margin "1px")
      
    ((ancestor :.autocomplete :ul)
     :background-color "#ffffff"
     :border "1px solid #666666"
     :padding "0"
     :margin "0")

    ((ancestor :.autocomplete :li) :list-style-type "none" :display "block" :padding ".2em")

    ((ancestor :.autocomplete :li.selected) :background-color "#0066ff" :color "#dddddd")

    ;;; Tabs
    (:ul.tabs
     :text-align "left" ; set to left, right or center
     :margin ".5em 0 .5em 0" ; set margins as desired
     :font-weight "bold"
     :border-bottom "1px solid #6c6" ;set border COLOR as desired
     :list-style-type "none"
     :padding "3px .2em 3px .2em") ; THIRD number must change with respect to padding-top (X) below

    ((ancestor :ul.tabs :li)
     :display "inline")

    ((ancestor :ul.tabs :li.selected)
     :border-bottom "1px solid #fff" ; set border color to page background color
     :background-color "#fff") ; set background color to match above border color

    ((ancestor :ul.tabs :li.selected :a)
     :background-color "#fff" ; set selected tab background color as desired
     :color "#000" ; set selected tab link color as desired
     :position "relative"
     :top "1px"
     :padding-top "4px") ;must change with respect to padding (X) above and below

    ((ancestor :ul.tabs :li :a)
     :padding "3px .7em" ; set padding (tab size) as desired; FIRST number must change with respect to padding-top (X) above
     :border "1px solid #6c6"  ; set border COLOR as desired; usually matches border color specified in #tabnav
     :background-color "#cfc" ; set unselected tab background color as desired
     :color "#666" ; set unselected tab link color as desired
     :margin-right "3px" ; set additional spacing between tabs as desired
     :text-decoration "none"
     :border-bottom "none")

    ((ancestor :ul.tabs :a\:hover)
     :background "white")
    
    ;; modal view
    (:.modal-background :position "absolute"
			:top 0
			:background-color "#000000"
			:width "100%"
			:z-index "99998"
			:height "550px"
			:filter "alpha(opacity=75)"
			:-moz-opacity "0.75"
			:-khtml-opacity "0.75"
			:opacity "0.75"
			)
    
    (:.modal :width "800px"
	     ;:height "500px"
	     :position "absolute"
	     :top "50px"
	     :left "50%"
	     :margin-left "-400px"
	     :z-index "99999"
	     ;:background-color "#ffffff"
	     )
    
    ;; snazzy form
    ((or (ancestor :.snazzy-form :p)
	 (ancestor :.snazzy-form :h1)
	 (ancestor :.snazzy-form :form)
	 (ancestor :.snazzy-form :.form-field :input))
     :border 0 :margin 0 :padding 0)

    (:.spacer :clear "both")

    (:.snazzy-form :min-height "1")
    ((ancestor :.snazzy-form :form) :min-height "0" :width "100%")
    ((ancestor :.snazzy-form :.floatholder) :min-height "0" :overflow "auto")

    (:.snazzy-form :padding "14px" :border "solid 2px #b7ddf2" :background-color "#ebf4fb")

    ((ancestor :.snazzy-form :h1) :font-weight "bold" :margin-bottom ".3em")

    ((ancestor :.snazzy-form :p)
     :font-size "80%" :color "#666666"  :margin-bottom "2em" :padding-bottom "1em"
     :border-bottom "solid 1px #b7ddf2")
    
    ((ancestor :.snazzy-form :.form-field)
     :overflow "auto"
     :margin "0 0 .7em 0")

    ((ancestor :.snazzy-form :label)
     :font-weight "bold" :text-align "right" :width "10em"
     :float "left"
     :display "inline") ; potential ie6 fix  inline is automatically converted to block by all browsers

    ((ancestor :.snazzy-form :label :.note)
     :display "block" :font-weight "normal" :font-size "80%" :text-align "right" :color "#666666")

    ((ancestor :.snazzy-form :.input)
     :margin "2px 0 0 10.5em" :display "block" :padding "0 0 .2em 0")

    ((ancestor :.snazzy-form  :.form-field :input)
     :border "solid 1px #aacfe4")

    ((ancestor :.snazzy-form :.form-field :input)
     ;:width "18em"
     :padding ".15em")
    
    ((ancestor :.snazzy-form :.button-holder)
     :margin-left "10.65em")

    ((ancestor :.snazzy-form :.button-holder :input)
     :margin-right "1.0em")

    #+nil
    ((ancestor :.snazzy-form :.button-holder :input)
     :background "#666666" :text-align "center" :line-height "2em"
     :color "#FFFFFF"  :font-weight "bold" :padding ".25em 1.5em .25em 1.5em"
     :margin-right "1em")
    
    #+nil
    ((ancestor :.snazzy-form :.button-holder :input.cancel)
     :background "#AA2222" :text-align "center" :line-height "2em"
     :color "#FFFFFF"  :font-weight "bold" :padding ".25em 1.5em .25em 1.5em"
     :margin-right "1em")

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
    
    
    
    ))
    