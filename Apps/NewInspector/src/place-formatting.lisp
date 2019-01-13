(cl:in-package #:new-inspector)

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
