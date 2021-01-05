;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2002,2003 Timothy Moore <tmoore@common-lisp.net>
;;;  (c) copyright 2008 Troels Henriksen <thenriksen@common-lisp.net>
;;;  (c) copyright 2019 admich <andrea.demichele@gmail.com>
;;;  (c) copyright 2019,2020,2021 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Frame pointer documentation computation and updating.

(in-package #:clim-internals)

(defgeneric frame-update-pointer-documentation
    (frame input-context stream event))

;;; Give a coherent order to sets of modifier combinations.  Multi-key combos
;;; come after single keys.

(defun cmp-modifiers (a b)
  (let ((cnt-a (logcount a))
        (cnt-b (logcount b)))
    (cond ((eql cnt-a cnt-b)
           (< a b))
          (t (< cnt-a cnt-b)))))

;;; XXX Warning: Changing rapidly!
;;;
;;; We don't actually want to print out the translator documentation and redraw
;;; the pointer documentation window on every motion event.  So, we compute a
;;; state object (basically modifier state and a list of the applicable
;;; presentation, translator and input context on each mouse button),
;;; compare it to the previous state object, and only write out documentation
;;; if they are different.  I suppose it's possible that this state object
;;; doesn't capture all possible documentation changes -- the doc generator is
;;; a function, after all -- but that's just tough.
;;;
;;; It would be nice to evolve this into a protocol so that elements other than
;;; presentations -- menu choices, for example -- could influence pointer
;;; documentation window.

(defgeneric frame-compute-pointer-documentation-state
    (frame input-context stream event)
  (:documentation
   "Compute a state object that will be used to generate pointer documentation."))

(defmethod frame-compute-pointer-documentation-state
    ((frame standard-application-frame) input-context stream event)
  (let* ((current-modifier (event-modifier-state event))
         (x (device-event-x event))
         (y (device-event-y event))
         (presentation (stream-output-history stream))
         (new-translators
           (loop for (nil button) in *pointer-buttons*
                 for context-list = (multiple-value-list
                                     (find-innermost-presentation-match
                                      input-context presentation frame
                                      stream x y event
                                      :override (list :button button)))
                 when (first context-list)
                 collect (list* button context-list))))
    (list current-modifier new-translators)))

(defgeneric frame-compare-pointer-documentation-state
    (frame input-context stream old-state new-state))

(defmethod frame-compare-pointer-documentation-state
    ((frame standard-application-frame) input-context stream
     old-state new-state)
  (declare (ignore input-context stream))
  (equal old-state new-state))

(defun record-on-display (stream record)
  "Return true if `record' is part of the output history of
`stream', false otherwise."
  (labels ((worker (record)
             (or (eq record (stream-output-history stream))
                 (and (not (null (output-record-parent record)))
                      (worker (output-record-parent record))))))
    (worker record)))

(defgeneric frame-print-pointer-documentation
    (frame input-context stream state event))

(defvar *background-message-minimum-lifetime* 1
  "The amount of seconds a background message will be kept
alive.")

(defmethod frame-print-pointer-documentation
    ((frame standard-application-frame) input-context stream state event)
  (unless state
    (return-from frame-print-pointer-documentation nil))
  (destructuring-bind (current-modifier new-translators)
      state
    (let ((x (device-event-x event))
          (y (device-event-y event))
          (pstream *pointer-documentation-output*))
      (if (null new-translators)
          (when (and (background-message pstream)
                     (not (record-on-display pstream (background-message pstream))))
            (cond ((> (get-universal-time)
                      (+ (background-message-time pstream)
                         *background-message-minimum-lifetime*))
                   (setf (background-message pstream) nil))
                  (t
                   (setf (output-record-parent (background-message pstream)) nil)
                   (stream-add-output-record pstream (background-message pstream))
                   (replay (background-message pstream) pstream))))
          (loop for (button presentation translator context)
                in new-translators
                for first-one = t then nil
                do (progn
                     (unless first-one
                       (write-string "; " pstream))
                     (format-pointer-gesture (list :pointer button current-modifier)
                                             :stream pstream)
                     (format pstream ": ")
                     (document-presentation-translator translator
                                                       presentation
                                                       (input-context-type context)
                                                       *application-frame*
                                                       event
                                                       stream
                                                       x y
                                                       :stream pstream
                                                       :documentation-type
                                                       :pointer))
                finally (when new-translators
                          (write-char #\. pstream))))
      ;; Wasteful to do this after doing ... something (used to be
      ;; find-innermost-presentation-context) above... look at doing
      ;; this first and then doing the innermost test.
      (let ((other-modifiers '()))
        (map-applicable-translators
         (lambda (translator presentation context)
           (declare (ignore presentation context))
           (let ((gesture (gesture translator)))
             (unless (eq gesture t)
               (loop for (name type modifier) in gesture
                     unless (or (eq modifier t)
                                (eql modifier current-modifier))
                     do (pushnew modifier other-modifiers)))))
         (stream-output-history stream) input-context *application-frame* stream
         x y nil :menu t :override '(:button nil :modifier-state nil))
        (when other-modifiers
          (terpri pstream)
          (write-string "To see other commands, press " pstream)
          (loop for (first-modifier . rest-modifiers)
                on (sort other-modifiers #'cmp-modifiers)
                for count from 0
                do (if (null rest-modifiers)
                       (progn
                         (when (> count 1)
                           (write-char #\, pstream))
                         (when (> count 0)
                           (write-string " or " pstream)))
                       (when (> count 0)
                         (write-string ", " pstream)))
                   (format-gesture-modifiers
                    first-modifier :stream pstream :print-nothing t))
          (write-char #\. pstream))))))

(defmethod frame-update-pointer-documentation
    ((frame standard-application-frame) input-context stream event)
  (when *pointer-documentation-output*
    (with-accessors ((frame-documentation-state frame-documentation-state)
                     (documentation-record documentation-record))
        frame
      (setf frame-documentation-state
            (frame-compute-pointer-documentation-state frame
                                                       input-context
                                                       stream
                                                       event))
      ;; These ugly special bindings work around the fact that the outer
      ;; updating-output form closes over its body and allow the inner
      ;; form to see the correct, current values of those variables.
      (let ((%input-context% input-context)
            (%stream% stream)
            (%doc-state% frame-documentation-state)
            (%event% event))
        (declare (special %input-context% %stream% %doc-state% %event%))
        (if (and documentation-record
                 (output-record-parent documentation-record))
            (redisplay documentation-record *pointer-documentation-output*)
            (progn
              (setf documentation-record
                    (updating-output (*pointer-documentation-output*)
                      (updating-output (*pointer-documentation-output*
                                        :cache-value %doc-state%
                                        :cache-test #'equal)
                        (frame-print-pointer-documentation frame
                                                           %input-context%
                                                           %stream%
                                                           %doc-state%
                                                           %event%))))))))))

(defgeneric invoke-with-output-to-pointer-documentation (frame continuation)
  (:documentation "Invoke CONTINUATION with a single argument -
a stream that the continuation can write to, the output of which
will be used as the background message of the pointer
documentation pane of FRAME. If the pointer-documentation of
FRAME is not a `pointer-documentation-pane', CONTINUATION
will not be called."))

(defmethod invoke-with-output-to-pointer-documentation
    ((frame standard-application-frame) continuation)
  (with-accessors ((pointer-documentation frame-pointer-documentation-output)) frame
    (when (typep pointer-documentation 'pointer-documentation-pane)
      (setf (background-message pointer-documentation)
            (with-output-to-output-record (pointer-documentation)
              (funcall continuation pointer-documentation))
            (background-message-time pointer-documentation) (get-universal-time)))))

(defmacro with-output-to-pointer-documentation ((stream frame) &body body)
  "Bind STREAM to the pointer-documentation pane of FRAME and
capture the output of BODY on STREAM as the background
message of the pointer documentation pane. If FRAME does not
have a `pointer-documentation-pane' as pointer documentation,
BODY will not be evaluated."
  `(invoke-with-output-to-pointer-documentation
    ,frame (lambda (,stream) ,@body)))

;;; A hook for applications to draw random strings in the
;;; *pointer-documentation-output* without screwing up the real pointer
;;; documentation too badly.

(defun frame-display-pointer-documentation-string (frame string)
  (with-output-to-pointer-documentation (stream frame)
    (write-string string stream))
  (let ((*pointer-documentation-output* (frame-pointer-documentation-output frame)))
    ;; To see the string it is necessary to trigger the redisplay of
    ;; pointer-documentation-pane with FRAME-UPDATE-POINTER-DOCUMENTATION.
    ;; As INPUT-CONTEXT we pass NIL. FRAME-COMPUTE-POINTER-DOCUMENTATION-STATE and
    ;; FRAME-PRINT-POINTER-DOCUMENTATION specialize on that.
    ;; We pass the STRING as EVENT argument in this way
    ;; FRAME-COMPUTE-POINTER-DOCUMENTATION-STATE calculate a new state
    ;; value cached for icremental-redisplay machinery.  -- admich 2019-11-15
    (frame-update-pointer-documentation frame nil nil string)))

(defmethod frame-compute-pointer-documentation-state
    ((frame standard-application-frame) (input-context null) stream event)
  (list :string event))

(defmethod frame-print-pointer-documentation
    ((frame standard-application-frame) (input-context null) stream state event)
  (unless state
    (return-from frame-print-pointer-documentation nil))
  (let ((pstream *pointer-documentation-output*))
    (when-let ((message (background-message pstream)))
      (cond ((record-on-display pstream message))
            ((> (get-universal-time)
                (+ (background-message-time pstream)
                   *background-message-minimum-lifetime*))
             (setf (background-message pstream) nil))
            (t
             (setf (output-record-parent message) nil)
             (stream-add-output-record pstream message)
             (replay message pstream))))))
