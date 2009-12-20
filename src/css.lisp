(in-package :iodb-ui)

(defparameter *tab-border-color* "#fda966")

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
     :border-bottom (format nil "1px solid ~A" *tab-border-color*) ;#ffc89c" ;set border COLOR as desired
     :list-style-type "none"
     :padding "3px .4em 3px .5em") ; THIRD number must change with respect to padding-top (X) below

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
     :border (format nil "1px solid ~A" *tab-border-color*)   ; set border COLOR as desired; usually matches border color specified in #tabnav
     :background-color "#ffe8d7" ; set unselected tab background color as desired
     :color "#666" ; set unselected tab link color as desired
     :margin-right "7px" ; set additional spacing between tabs as desired
     :text-decoration "none"
     :border-bottom "none")

    ((ancestor :ul.tabs :a\:hover)
     :background "white"))) ;/* set desired hover color */

