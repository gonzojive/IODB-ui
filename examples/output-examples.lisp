(require :iodb-ui)
(require :cl-who)

(defpackage :iodb-examples
    (:use :iodb-ui :cl :paren-psos :parenscript :paren-util :cl-who))

(in-package :iodb-examples)

(defun example-pathname (relative-pathname)
  (merge-pathnames relative-pathname
                   (asdf:system-relative-pathname (asdf:find-system :iodb-ui)
                                                  "examples/")))

(defmacro with-example-file ((stream-var filename) &body body)
  `(with-open-file (,stream-var (example-pathname ,filename) :direction :output :if-exists :supersede)
     ,@body))

(defun output-examples ()
  (with-example-file (s "iodb-ui.js")
    (paren-files:compile-script-system (asdf:find-system :iodb-ui) :output-stream s))

  (with-example-file (s "example1.html")
    (with-html-output (s)
      (:html
       (:head (:title "Example 1")
              (:script :type "text/javascript" :src "iodb-ui.js")
              (:script :type "text/javascript" :src "example1.js"))
       (:body "Will be replaced...")))))
