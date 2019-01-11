(cl:in-package #:new-inspector)

;;; Utilities

(defun call-with-preserved-cursor-x (thunk stream)
  (let ((old-x (stream-cursor-position stream)))
    (prog1
        (funcall thunk stream)
      (setf (stream-cursor-position stream)
            (values old-x (nth-value 1 (stream-cursor-position stream)))))))

(defmacro with-preserved-cursor-x ((stream) &body body)
  (check-type stream symbol)
  `(call-with-preserved-cursor-x (lambda (,stream) ,@body) ,stream))

;;; Styles

(defmacro with-style ((stream style &rest extra-drawing-options &key)
                      &body body)
  (let ((args (ecase style
                (:header    '(:text-face :bold))
                (:changable '(:ink +dark-violet+))
                (:slot-like '(:ink +dark-orange+ :text-size :small)))))
    `(with-drawing-options (,stream ,@args ,@extra-drawing-options)
       ,@body)))

(defun call-with-section (body-thunk title-thunk stream)
  (with-preserved-cursor-x (stream)
    (with-style (stream :header)
      (funcall title-thunk stream))
    (fresh-line stream)
    (indenting-output (stream "    ")
      (funcall body-thunk stream)
      #+TODO-this-illustrates-the-indentation-problem (progn
        (format stream "start~%")
        (format stream "start~%")
        (with-preserved-cursor-x (stream) (funcall body-thunk stream))
        (format stream "end~%")
        (format stream "end~%")))))

(defmacro with-section ((stream) title &body body)
  (check-type stream symbol)
  `(call-with-section
    (lambda (,stream) ,@body)
    (lambda (,stream)
      ,(typecase title
         (string `(write-string ,title ,stream))
         (t      title)))
    ,stream))

;;; Formatting places

(defun call-with-place-inspector (thunk state place-class stream)
  (flet ((make-place (cell)
           (make-instance place-class :container state :cell cell))
         (present-place (place)
           (present place 'place :stream stream))
         (inspect-place (place)
           (let ((*place* place))
             (inspect-object (value place) stream))))
    (funcall thunk #'make-place #'present-place #'inspect-place)))

(defmacro with-place-inspector ((state place-class stream) (make inspect present) &body body)
  (with-gensyms (make-var inspect-var present-var)
    `(call-with-place-inspector
      (lambda (,make-var ,inspect-var ,present-var)
        (flet ((,make (cell)
                 (funcall ,make-var cell))
               (,inspect (place)
                 (funcall ,inspect-var place))
               (,present (place)
                 (funcall ,present-var place)))
          ,@body))
      ,state ,place-class ,stream)))

(defmacro formatting-place-list ((container place-class stream &rest args &key) &body body)
  (once-only (container place-class stream)
    `(formatting-item-list (stream ,@args)
       (macrolet ((formatting-place-cell (stream-var cell place-var inspect present)
                      `(formatting-cell (,stream-var)
                         (let ((,place-var (ensure-child ,cell ,place-class *parent-place*
                                                         (lambda ()
                                                           (make-instance place-class
                                                                          :container ,container
                                                                          :cell      ,cell)))))
                           (flet ((,present (stream)
                                    (present ,place-var 'place :stream stream))
                                  (,inspect (stream)
                                    (let ((*place* ,place-var))
                                      (inspect-object )))))))))
         ,@body))))

(defmacro formatting-place-cell ((stream &rest args)
                                 (container place-class cell present inspect
                                  &key (place-var (gensym "PLACE")))
                                 &body body)
  (once-only (container place-class stream)
    `(formatting-cell (,stream ,@args)
       (let ((,place-var (ensure-child ,cell ,place-class *parent-place*
                                       (lambda ()
                                         (make-instance ,place-class
                                                        :container ,container
                                                        :cell      ,cell)))))
         (flet ((,present (&optional (stream ,stream))
                  (present ,place-var 'place :stream stream))
                (,inspect (&optional (stream ,stream))
                  (let ((*place* ,place-var))
                    (inspect-object (value ,place-var) stream))))
           ,@body)))))

(defmacro formatting-place ((stream container place-class cell present inspect
                             &key (place-var (gensym "PLACE")))
                            &body body)
  (once-only (container place-class stream)
    `(let ((,place-var (ensure-child ,cell ,place-class *parent-place*
                                     (lambda ()
                                       (make-instance ,place-class
                                                      :container ,container
                                                      :cell      ,cell)))))
       (flet (,@(when present
                  `((,present (&optional (stream ,stream))
                              (present ,place-var 'place :stream stream))))
              ,@(when inspect
                  `((,inspect (&optional (stream ,stream))
                              (inspect-place ,place-var stream)))))
         ,@body))))
