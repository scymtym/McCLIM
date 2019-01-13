(cl:in-package #:new-inspector)

(define-command-table inspector)

;;; Commands on all inspected objects

(define-command (com-expand :command-table inspector
                            :name          t)
    ((object inspected-object))
  (setf (style object) :expanded))

(define-presentation-to-command-translator object->expand
    (inspected-object com-expand inspector
     :tester ((object) (not (eq (style object) :expanded)))
     :documentation "Expand object"
     :pointer-documentation
     ((object stream)
      (format stream "Expand ~A" (ignore-errors (object object))))) ; TODO make a safe printer; use here and for brief style
    (object)
  (list object))

(define-command (com-collapse :command-table inspector
                              :name          t)
    ((object inspected-object))
  (setf (style object) :brief))

(define-presentation-to-command-translator object->collapse
    (inspected-object com-collapse inspector
     :tester ((object) (eq (style object) :expanded))
     :documentation "Collapse object"
     :pointer-documentation
     ((object stream)
      (format stream "Collapse ~A" (ignore-errors (object object))))) ;TODO
    (object)
  (list object))

;;; Commands on all places

(defun call-with-command-error-handling (do-thunk undo-thunk
                                         &optional format-control
                                         &rest format-arguments)
  (handler-case
      (funcall do-thunk)
    (error (condition)
      (funcall undo-thunk)
      (let ((stream (frame-standard-output *application-frame*)))
        (with-style (stream :error)
          (format stream "~&~@<~?: ~A~@:>~%"
                  (or format-control "Error executing command")
                  format-arguments
                  condition))))))

(defmacro with-command-error-handling ((&optional format-control
                                        &rest format-arguments)
                                       do-form &body undo-forms)
  `(call-with-command-error-handling
    (lambda () ,do-form) (lambda () ,@undo-forms)
    ,format-control ,@format-arguments))

(define-command (com-set-place :command-table inspector
                               :name          t)
    ((place 'place :prompt "Place to set value of"))
  (handler-case ; TODO use with-command-error-handling
      (setf (value place) (accept t :prompt "New place value")) ; TODO just second argument?
    (simple-parse-error ()
      (format (get-frame-pane *application-frame* 'int)
              "~&Command canceled; place value not set~%"))))

(define-presentation-to-command-translator place->com-set-place
    (place com-set-place inspector
     :gesture       :edit
     :tester        ((object) (supportsp object 'setf))
     :documentation "Set value of place")
    (object)
  (list object))

(define-command (com-make-place-unbound :command-table inspector ; TODO remove place value
                                        :name          t)
    ((place 'place :prompt "Place to remove value of"))
  (with-command-error-handling
      ("Could not remove value of place ~A" place)
      (make-unbound place)))

(define-presentation-to-command-translator place->com-remove-place-value
    (place com-make-place-unbound inspector
     :gesture       :delete
     :tester        ((object)
                     (and (supportsp object 'remove-value)
                          (valuep object)))
     :documentation "Remove value of place")
    (object)
  (list object))

(define-command (com-copy-place-value :command-table inspector
                                      :name          t)
    ((from-place 'place :prompt "From place")
     (to-place   'place :prompt "To place"))
  (let ((old-value (value to-place))
        (new-value (value from-place)))
    (with-command-error-handling
        ("Could not copy value from ~A to ~A" from-place to-place)
        (progn
          (setf (value to-place) new-value
                (state to-place) (make-object-state new-value to-place)))
      (setf (value to-place) old-value))))

(define-drag-and-drop-translator drag-copy-place-value
    (place command place inspector
     :gesture :select
     :tester ((object from-object)
              (cond ((not from-object)
                     (valuep object)) ; TODO should work for unbound?
                    ((eq from-object object)
                     nil)
                    ((valuep from-object)
                     (ignore-errors ; TODO do this properly
                      (and (supportsp object 'setf)
                           (accepts-value-p object (value from-object)))))
                    (t
                     (supportsp object 'remove-value))))
     :pointer-documentation ((object destination-object stream)
                             (if destination-object
                                 (format stream "Copy value of ~A into ~A"
                                         object destination-object)
                                 (format stream "Drag onto place to ~
                                                 copy value of ~A"
                                         object))))
    (object destination-object)
  (list 'com-copy-place-value object destination-object))

(define-command (com-swap-place-values :command-table inspector
                                       :name          t)
    ((place-1 'place :prompt "First place")
     (place-2 'place :prompt "Second place"))
  ;; Attempt to change values (without children and states) first so
  ;; that fewer things need undoing if, for example, a slot type check
  ;; signals an error.
  (let ((old-value-1 (value place-1))
        (old-value-2 (value place-2)))
    (with-command-error-handling
        ("Could not swap ~A and ~A" place-1 place-2)
        (progn
          (setf (value place-1) old-value-2
                (value place-2) old-value-1)
          (rotatef (children place-1) (children place-2))
          (rotatef (state place-1)    (state place-2)))
      (setf (value place-1) old-value-1
            (value place-2) old-value-2))))

(define-gesture-name :swap :pointer-button-press (:left :control))

(define-drag-and-drop-translator drag-swap-place-values
    (place command place inspector
     :gesture :swap
     :tester ((object from-object)
              (cond ((not from-object)
                     (valuep object)) ; TODO should work for unbound?
                    ((eq from-object object)
                     nil)
                    ((valuep from-object)
                     (ignore-errors ; TODO do this properly
                      (and (supportsp object 'setf)
                           (accepts-value-p object (value from-object)))))
                    (t
                     (supportsp object 'remove-value))))
     :documentation
     ((object stream)
      (format stream "Drag ~A onto another slot to swap their contents."
              object))
     :pointer-documentation
     ((object destination-object stream)
      (if destination-object
          (format stream "Swap ~A and ~A"
                  object destination-object)
          (format stream "Drag onto place to swap with ~A"
                  object))))
    (object destination-object)
  (list 'com-swap-place-values object destination-object))

;;; Commands on Boolean-valued places

(define-command (com-set-place-to-false :command-table inspector
                                        :name          t)
    ((place 'place))
  (with-command-error-handling
      ("Could not set value of ~A to false" place)
      (setf (value place) nil)))

(define-presentation-to-command-translator place->com-set-place-to-false
    (place com-set-place-to-false inspector
     :tester ((object)
              (and (supportsp object 'setf)
                   (accepts-value-p object t)
                   (valuep object)
                   (eq (value object) t)))
     :documentation "Set to false"
     :pointer-documentation
     ((object stream)
      (format stream "Set value of ~A to ~S" object nil)))
    (object)
  (list object))

(define-command (com-set-place-to-true :command-table inspector
                                       :name          t)
    ((place 'place))
  (with-command-error-handling
      ("Could not set value of ~A to true" place)
      (setf (value place) t)))

(define-presentation-to-command-translator place->com-set-place-to-true
    (place com-set-place-to-true inspector
     :tester ((object)
              (and (supportsp object 'setf)
                   (accepts-value-p object t)
                   (valuep object)
                   (eq (value object) nil)))
     :documentation "Set to true"
     :pointer-documentation
     ((object stream)
      (format stream "Set value of ~A to ~S" object t)))
    (object)
  (list object))

;;; Commands on real-valued places

(define-command (com-increment-place :name          "Increment"
                                     :command-table inspector)
    ((place 'place))
  (with-command-error-handling
      ("Could not increment the value of ~A" place)
      (incf (value place))))

(define-command (com-decrement-place :name          "Decrement"
                                     :command-table inspector)
    ((place 'place))
  (with-command-error-handling
      ("Could not decrement the value of ~A" place)
      (decf (value place))))

(clim:define-gesture-name :increment :pointer-scroll (:wheel-up :control))

(define-presentation-to-command-translator place->com-inrement-place
    (place com-increment-place inspector
     :gesture :increment
     :tester ((object)
              (and (supportsp object 'setf)
                   (valuep object)
                   (let ((value (value object)))
                     (and (typep value 'real)
                          (accepts-value-p object (1+ value))))))
     :documentation "Increment by 1"
     :pointer-documentation
     ((object stream)
      (format stream "Increment ~A by 1" object)))
    (object)
  (list object))

(clim:define-gesture-name :decrement :pointer-scroll (:wheel-down :control))

(define-presentation-to-command-translator place->com-decrement-place
    (place com-decrement-place inspector
     :gesture :decrement
     :tester ((object)
              (and (supportsp object 'setf)
                   (valuep object)
                   (let ((value (value object)))
                     (and (typep value 'real)
                          (accepts-value-p object (1- value))))))
     :documentation "Decrement by 1"
     :pointer-documentation
     ((object stream)
      (format stream "Decrement ~A by 1" object)))
    (object)
  (list object))
