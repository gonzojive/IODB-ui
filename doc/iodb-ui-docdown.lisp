(defpackage :iodb-ui.doc
    (:use :docdown :iodb-ui :cl :contextl :cl-who :parenscript :paren-util :paren-psos :cl-sails)
  (:export #:output-docs))

(in-package :iodb-ui.doc)

(declaim (optimize (debug 3)))

#+nil
(let ((*readtable* (copy-readtable nil)))
       (set-dispatch-macro-character #\# #\W
                                     (lambda (s c n)
                                       (let* ((pos (file-position s))
                                              (read-result (read s))
                                              (final-pos (file-position s))
                                              (literal (let ((seq (make-sequence 'string (- final-pos pos))))
                                                         (file-position s pos)
                                                         (read-sequence seq s)
                                                         seq)))
                                         (list literal pos final-pos read-result))))
       (read-from-string "#W\"hello, world!\""))

(defclass verbatim-form ()
  ((form :initarg :form :accessor verbatim-form)
   (literal :initarg :literal :accessor verbatim-literal)))

(defmethod make-load-form ((self verbatim-form) &optional environment)
   (declare (ignore environment))
   `(make-instance 'verbatim-form
                   :form ,(verbatim-form self)
                   :literal ,(verbatim-literal self)))

(defmethod print-object ((node verbatim-form) s)

           (format s "#W~A" (verbatim-literal node)))

(defun verbatim-reader (s c n)
  "Reads a lisp from from a stream and returns a list with the read
result and the literal value read."
  (declare (ignore n c))
  (let* ((pos (file-position s))
         (read-result (read s))
         (final-pos (file-position s))
         (literal (let ((seq (make-sequence 'string (- final-pos pos))))
                    (file-position s pos)
                    (read-sequence seq s)
                    seq)))
    (make-instance 'verbatim-form :form read-result :literal literal)))

(set-dispatch-macro-character #\# #\W 'verbatim-reader)


(define-layered-class parenscript-example-docnode
  (abstract-docnode)
  ((js-eval? :initarg :js-eval? :initform nil :accessor docnode-js-eval?
             :documentation "True of the parenscript form should be evaluated.")
   (verbatim-form :initarg :verbatim-form :initarg :code
                  :initform nil :accessor docnode-verbatim-form
                  :documentation "The VERBATIM-FORM involved.")))

(register-docnode-class (find-class 'parenscript-example-docnode) :aliases '(:parenscript))
(define-option-evaluator :code (value)
  (format t "Option evaluator!~%")
  (list :code value))
(define-option-evaluator :js-eval? (value)
  (list :js-eval? value))

(defun split-lines (str)
  "Returns a list of the lines in a string."
  (ppcre:split (ppcre:create-scanner (format nil "~%") :multi-line-mode t)
               str))

(define-layered-method doc
  :in-layer html-generation-layer ((node parenscript-example-docnode) &key &allow-other-keys)
  (assert (listp (verbatim-form (docnode-verbatim-form node))))
  (cl-who:with-html-output-to-string (s)
    (let* ((literal-string (verbatim-literal (docnode-verbatim-form node)))
           (lines (split-lines (subseq literal-string  1 (1- (length literal-string)))))
           (javascript (when (docnode-js-eval? node)
                         (apply 'parenscript:ps* (verbatim-form (docnode-verbatim-form node))))))
      (when (equal "" (first lines))
        (setf lines (cdr lines)))
      (let ((num-common-leading-spaces
             (apply #'min (mapcar #'(lambda (line)
                                      (or (position #\Space line :test (complement #'char=))
                                          (length line)))
                                  lines))))
        (htm
         (:pre
          #+nil
          (:code :class "paren-translation"
                 (esc javascript))
          (:code (esc (let ((refined-literal
                                  (format nil "~{~A~^~%~}"
                                          (mapcar #'(lambda (line) (subseq line num-common-leading-spaces))
                                                  lines))))
                             refined-literal
                             ;(colorize:format-scan :common-lisp (colorize:scan-string :common-lisp refined-literal))
                             ))))))
      (when (docnode-js-eval? node)
        (htm
         (:script :type "text/javascript"
                  (fmt "//<![CDATA[~%~A//]]>" javascript)))))))

(progn
  (defdoc index :page
    (:title "IODB.org UI (iodb-ui)")
    (:systems :iodb-ui); :cl-tidy.doc)
    (:content
;     "#### [HTML Tidy](http://sourceforge.net/projects/tidy/) for Common Lisp
     (defdoc scripts :html
       (:html
        (with-html-output-to-string (s)
          (:script :type "text/javascript"
                   (fmt "//<![CDATA[~%")
                   (str (with-output-to-string (s)
                          (paren-files:compile-script-system (asdf:find-system :iodb-ui) :output-stream s)))
                   (str "//]]>"))
          (:style :type "text/css"
                  (iodb-ui:output-css  s)
                  ;(esc colorize:*coloring-css*)
                  (css-sexp:with-css-output (s)
                    (:pre :overflow "auto")
                    (:code.paren-translation :float "right" :width "40%" :background-color "#eee" :border "1px solid #aaa")
                    (:h2 :border-bottom "1px dashed #bbb"))))))
     "
## Synopsis

IODB-UI is a set of Javascript user interface components for the
cl-sails framework.  The following user interfaces elements are
included:

* autocomplete
* modal views (i.e. components that monopolize user input until they go away, like dialog boxes)
* Google maps syntax sugar
* 'snazzy' forms
* multicomplete (a la facebook friend finder) 
* browser detection
* tabs

currently this documentation sucks.
")

    (:sections
     (defdoc download :section
       (:title "Download and Installation")
       (:content "All the code is maintained in a git repository.  To
obtain the library, use the following command:

    git clone git://github.com/gonzojive/iodb-ui.git

You can also browse the code at [http://github.com/gonzojive/iodb-ui](http://github.com/gonzojive/iodb-ui).
"))
     (defdoc tutorial :section
       (:title "Tutorial & Demos")
       (:content "iodb-ui packages a few generic, reusable components
       for use in javascript.  Here are some examples of basic usage (and demos!).")

       (:children
        (defdoc autocomplete-example1 :standard
          (:title "Autocomplete example")
          (:content
;           "Hello, world!  here's some html..."
           "An autocomplete input box looks like this:"

           (defdoc button-for-modal-view-example1 :html
             (:html "<form><input id='autocomplete1' type='text' /></form>"))

           (defdoc modal-view-example1-code-part1 :parenscript
             (:js-eval? t)
             (:code
              #W(
                 (autocomplete-input (elem-by-id "autocomplete1")
                                     :entered-item-callback (lambda (value display-string)
                                                              (js-global::alert (+ "Entered " value)))
                                     :data-callback (lambda (input-value)
                                                      (let ((options (list (list "1" "One")
                                                                           (list "2" "Two")
                                                                           (list "3" "Three")
                                                                           (list "4" "Four")
                                                                           (list "5" "Five")
                                                                           (list "6" "Six"))))
                                                        (return (methcall "filter"
                                                                          options
                                                                          (lambda (option)
                                                                            (return (!== -1 (methcall "indexOf" (elt option 0) input-value))))))))))))
           ))


	(defdoc modal-view-example1 :standard
          (:title "Modal view example")
          (:content
;           "Hello, world!  here's some html..."
           "A 'modal' view monopolizes user input and attention.  A
common example of this is the <code>alert()</code> function in
Javascript.  iodb-ui provides a different facility that darkens the
entire page and displays a custom sail.

First we define a custom sail:"

           (defdoc modal-view-example1-code-part1 :parenscript
             (:js-eval? t)
             (:code
              #W(
                 (defsail little-alert ()
                   ()
                   (:css ".mydiv { width: 10em; margin: auto; height: 5em; background-color: white; padding:1em; }")
                   (:html "<div class='mydiv' > This will close in 2 seconds. </div>")))))

           "Next we show a function that presents a new instance of this sail as a modal view to the user:"

           (defdoc modal-view-example1-code-part2 :parenscript
             (:js-eval? t)
             (:code
              #W(
                 (defun display-little-alert ()
                   (let ((sail (make-instance little-alert)))
                     (present-modal-sail sail)
                     (js-global::set-timeout (lambda () (dismiss-modal-sail sail))
                                             2000))))))

           (defdoc button-for-modal-view-example1 :html
             (:html "<button onclick='displayLittleAlert()' type='button'>Click to see a modal view alert</button>"))

           

           ))))


     (defdoc functions :section
       (:title "Functions")
       (:content "")
       (:children
	(defdoc iodb-ui:output-css :function)))
     ))
  (output-docs))

(defun output-docs ()
  (with-open-file (stream (asdf:system-relative-pathname (asdf:find-system :iodb-ui.doc)
							 "doc/index.html")
			  :direction :output :if-exists :supersede)
    (write-string (generate-html-page 'index) stream)))
   
  