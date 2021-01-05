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
   "Compute a state object that will be used to generate pointer
documentation."))

(defgeneric frame-compare-pointer-documentation-state
    (frame input-context stream old-state new-state))

(defgeneric frame-print-pointer-documentation
    (frame input-context stream state event))

(defgeneric frame-update-pointer-documentation
    (frame input-context stream event))

(defmethod frame-compute-pointer-documentation-state
    ((frame standard-application-frame) input-context stream event)
  (let* ((current-modifiers (event-modifier-state event))
         (x                 (device-event-x event))
         (y                 (device-event-y event))
         (result            '())
         (other-modifiers   '()))
    (labels ((consider-gesture (button modifiers translator presentation context size)
               (cond ((or (eq modifiers t)
                          (eql modifiers current-modifiers))
                      (let* ((existing         (alexandria:assoc-value result button))
                             (existing-context (third existing))
                             (existing-size    (fourth existing)))
                        (when (or (not existing)
                                  (and (eq context existing-context)
                                       (< size existing-size)))
                          (setf (alexandria:assoc-value result button)
                                (list presentation translator context size)))))
                     ((eq (menu translator) t)
                      (pushnew modifiers other-modifiers))))
             (consider-translator (translator presentation context size)
               (let ((gesture (gesture translator)))
                 (unless (eq gesture t)
                   (loop for (nil button modifiers) in gesture
                         do (consider-gesture
                             button modifiers translator presentation context size))))))
      ;; Look for applicable translators using all "ordinary"
      ;; presentation.
      (map-applicable-translators
       (lambda (translator presentation context)
         (multiple-value-bind (min-x min-y max-x max-y)
             (output-record-hit-detection-rectangle* presentation)
           (let ((size (* (- max-x min-x) (- max-y min-y))))
             (consider-translator translator presentation context size))))
       (stream-output-history stream) input-context frame stream x y event
       :override '(:modifier-state nil :button nil))
      ;; Look for applicable translators using the blank area
      ;; presentation.
      (map-applicable-translators
       (lambda (translator presentation context)
         (consider-translator translator presentation context most-positive-fixnum))
       (make-blank-area-presentation stream event)
       input-context frame stream x y event
       :override '(:modifier-state nil :button nil)))
    (list current-modifiers (sort result #'< :key #'car) other-modifiers)))

(defmethod frame-compare-pointer-documentation-state
    ((frame standard-application-frame) input-context stream
     old-state new-state)
  (declare (ignore input-context stream))
  (equal old-state new-state))

(defun record-on-display (stream record)
  "Return true if `record' is part of the output history of
`stream', false otherwise."
  (let ((history (stream-output-history stream)))
    (labels ((worker (record)
               (or (eq record history)
                   (when-let ((parent (output-record-parent record)))
                     (worker parent)))))
      (worker record))))

(defvar *background-message-minimum-lifetime* 1
  "The amount of seconds a background message will be kept
alive.")

(defun invoke-formatting-pointer-documentation-table (stream continuation)
  (with-bounding-rectangle* (x1 y1 x2 y2) stream
    (declare (ignore x1 x2))
    (formatting-table (stream :x-spacing 8)
      (let ((output-count 0))
        (flet ((invoke-as-cell (continuation)
                 (formatting-column (stream)
                   (formatting-cell (stream)
                     (let ((x (stream-cursor-position stream)))
                       (when (plusp output-count)
                         (draw-line* stream x y1 x y2 :ink +gray40+ :line-thickness 2)
                         (incf x 8)
                         (stream-increment-cursor-position stream 8 0))
                       (incf output-count)
                       (with-temporary-margins (stream :left x)
                         (funcall continuation stream)))))))
          (funcall continuation stream #'invoke-as-cell))))))

(defmacro formatting-pointer-documentation-table ((stream) &body body)
  (check-type stream symbol)
  (alexandria:with-unique-names (cell)
    `(invoke-formatting-pointer-documentation-table
      ,stream (lambda (,stream ,cell)
                (declare (ignorable ,stream))
                (macrolet ((cell ((stream) &body body)
                             `(funcall ,',cell (lambda (,stream) ,@body))))
                  ,@body)))))

;;; Give a coherent order to sets of modifier combinations.  Multi-key combos
;;; come after single keys.
(defun compare-modifiers (modifiers1 modifiers2)
  (let ((count1 (logcount modifiers1))
        (count2 (logcount modifiers2)))
    (if (eql count1 count2)
        (< modifiers1 modifiers2)
        (< count1 count2))))

(defmethod frame-print-pointer-documentation
    ((frame standard-application-frame) input-context stream (state null) event)
  nil)

(defmethod frame-print-pointer-documentation
    ((frame standard-application-frame) input-context stream state event)
  (destructuring-bind (current-modifier new-translators other-modifiers) state
    (let ((pstream *pointer-documentation-output*)
          (frame *application-frame*))
      (if (null new-translators)
          (when-let ((message (background-message pstream)))
            (cond ((record-on-display pstream message)) ; already on display
                  ((> (get-universal-time)
                      (+ (background-message-time pstream)
                         *background-message-minimum-lifetime*))
                   (setf (background-message pstream) nil))
                  (t
                   (setf (output-record-parent message) nil)
                   (stream-add-output-record pstream message)
                   (replay message pstream))))
          (formatting-pointer-documentation-table (pstream)
            (loop with x = (device-event-x event)
                  with y = (device-event-y event)
                  for (button . (presentation translator context)) in new-translators
                  do (cell (pstream)
                       (format-pointer-gesture (list :pointer button current-modifier)
                                               :stream pstream)
                       (stream-increment-cursor-position pstream 8 0)
                       (with-temporary-margins (pstream :left (stream-cursor-position pstream))
                         (document-presentation-translator
                          translator presentation (input-context-type context)
                          frame event stream x y
                          :stream pstream :documentation-type :pointer))))
            (when other-modifiers
              (cell (pstream)
                (format pstream "To see other commands, press~%")
                (loop for (first-modifier . rest-modifiers)
                      on (sort other-modifiers #'compare-modifiers)
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
                (write-char #\. pstream))))))))

(defmethod frame-update-pointer-documentation
    ((frame standard-application-frame) input-context stream event)
  (when *pointer-documentation-output*
    (with-accessors ((frame-documentation-state frame-documentation-state)
                     (documentation-record documentation-record))
        frame
      (setf frame-documentation-state
            (frame-compute-pointer-documentation-state
             frame input-context stream event))
      ;; These ugly special bindings work around the fact that the outer
      ;; updating-output form closes over its body and allow the inner
      ;; form to see the correct, current values of those variables.
      (let ((%input-context% input-context)
            (%stream%        stream)
            (%doc-state%     frame-documentation-state)
            (%event%         event))
        (declare (special %input-context% %stream% %doc-state% %event%))
        (if (and documentation-record
                 (output-record-parent documentation-record))
            (redisplay documentation-record *pointer-documentation-output*)
            (setf documentation-record
                  (updating-output (*pointer-documentation-output*)
                    (updating-output (*pointer-documentation-output*
                                      :cache-value %doc-state%
                                      :cache-test #'equal)
                      (frame-print-pointer-documentation
                       frame %input-context% %stream% %doc-state% %event%)))))))))

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
