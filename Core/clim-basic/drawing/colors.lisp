;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2020 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Definitions of named colors based on data/colors.sexp which is in
;;; turn derived from the X11 color definitions (usually
;;; /usr/share/X11/rgb.txt).

(in-package #:clim-internals)

(let* ((file-pathname #.(or *compile-file-pathname*
                            *load-pathname*))
       (colors-pathname (merge-pathnames "../../../data/colors.sexp"
                                         file-pathname)))
  (with-open-file (stream colors-pathname :direction :input)
    (loop for (symbol-name pretty-name red green blue) = (read stream nil nil)
          while symbol-name
          ;; Black and white are defined in design.lisp
          unless (member pretty-name '("black" "white") :test #'string=)
            do (let ((symbol (find-symbol symbol-name '#:clim)))
                 (eval `(defvar ,symbol (make-named-color ,pretty-name ,red ,green ,blue)))))))
