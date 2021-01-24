;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2020,2021 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Compare the output of graphics operations to expected output read
;;; from raster image files.

(cl:in-package #:clim-test-util)

(defun rgba->char (rgba)
  (multiple-value-bind (intensity hue saturation)
      (color-ihs (multiple-value-bind (red green blue)
                     (mcclim-render-internals::%rgba->vals rgba)
                   (make-rgb-color (/ red 255) (/ green 255) (/ blue 255))))
    (cond ((>= saturation .7)
           (let ((hue (- hue 1/6)))
            (cond ((<= -1/6 hue 1/6) #\▀)
                  ((<= 1/6  hue 3/6) #\▌)
                  (t                 #\▄))))
          ((<= intensity (* 1/4 (sqrt 3))) #\░)
          ((<= intensity (* 2/4 (sqrt 3))) #\▒)
          ((<= intensity (* 3/4 (sqrt 3))) #\▓)
          (t                               #\█))))

(defun print-pattern (stream pattern &optional atp colonp)
  (declare (ignore atp colonp))
  (loop :with width = (pattern-width pattern)
        :with height = (pattern-height pattern)
        :with array = (clime:pattern-array pattern)
        :for y :below (or height 0)
        :do (loop :for x :below (or width 0)
                  :for color = (aref array y x)
                  :do (write-char (rgba->char color) stream))
            (terpri stream)))

(defun is-same-bitmap (expected-pattern actual-pattern &key description)
  (let* ((width       (pattern-width  expected-pattern))
         (height      (pattern-height expected-pattern))
         (differences (make-array (list height width)
                                  :element-type    '(unsigned-byte 32)
                                  :initial-element 0))
         (count       0))
    ;; Compare pixel first so me might get an indication of what the
    ;; problem is even if the dimensions do not match.
    (loop :with expected-array = (clime:pattern-array expected-pattern)
          :with actual-array   = (clime:pattern-array actual-pattern)
          :for y :below (min height (pattern-height actual-pattern))
          :do (loop :for x :below (min width (pattern-width actual-pattern))
                    :for expected-color = (aref expected-array y x)
                    :for actual-color = (aref actual-array y x)
                    :when (/= expected-color actual-color)
                    :do (incf count)
                        (setf (aref differences y x) #xffffffff)))
    (unless (zerop count)
      (fiveam:fail "~:[The~;~:*~A, the~] image differs from the ~
                    references image in ~D pixel~:P:~2%~
                    Reference:~@
                    ~/clim-test-util::print-pattern/~2%~
                    Actual:~@
                    ~/clim-test-util::print-pattern/~2%~
                    Difference:~@
                    ~/clim-test-util::print-pattern/"
                   description count
                   expected-pattern actual-pattern
                   (make-instance 'climi::%rgba-pattern :array differences)))
    (let ((actual-width (pattern-width actual-pattern)))
      (fiveam:is (= width actual-width)
                 "~:[The~;~:*~A, the~] width of the produced output is ~
                  ~D but expected ~D."
                 description actual-width width))
    (let ((actual-height (pattern-height actual-pattern)))
      (fiveam:is (= height actual-height)
                 "~:[The~;~:*~A, the~] height of the produced output is ~
                  ~D but expected ~D."
                 description actual-height height))))

(defun invoke-and-compare-to-reference (thunk reference &key description)
  (let* ((output    (mcclim-raster-image:with-output-to-rgba-pattern (stream)
                      (funcall thunk stream)))
         (directory #.(merge-pathnames "../reference-output/"
                                       (or *compile-file-pathname*
                                           *load-pathname*)))
         (filename  (make-pathname :name     reference
                                   :type     "png"
                                   :defaults directory))
         (reference (restart-case
                        (handler-bind ((error #'invoke-debugger))
                          (make-pattern-from-bitmap-file filename))
                      (write-reference ()
                        :report "Write test output as reference"
                        (climi::write-bitmap-file
                         output filename :format :png)
                        output))))
    (is-same-bitmap reference output :description description)))

(defmacro with-comparison-to-reference ((stream reference) &body body)
  `(invoke-and-compare-to-reference
    (lambda (,stream) ,@body)
    ,reference))
