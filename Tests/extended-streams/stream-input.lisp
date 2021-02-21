;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2020 Daniel Kochmański (daniel@turtleware.eu)
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Tests for stream input.

(in-package #:clim-tests)

(def-suite* :mcclim.extended-streams.stream-input
  :in :mcclim)

(test stream-read-gesture.smoke
  "Smoke test for `stream-read-gesture'."

  (let ((lame-event (make-instance 'pointer-event :sheet nil))
        (sis        (make-instance 'standard-input-stream))
        (seis       (make-instance 'standard-extended-input-stream)))
    ;; Initially nothing available.
    (is (null (climi::stream-gesture-available-p sis)))
    (is (null (climi::stream-gesture-available-p seis)))
    ;; Append characters and events.
    (finishes (climi::stream-append-gesture sis #\d))
    (signals error (climi::stream-append-gesture sis lame-event))
    (finishes (climi::stream-append-gesture seis #\d))
    (finishes (climi::stream-append-gesture seis lame-event))
    ;; Gestures available on both streams.
    (is (climi::stream-gesture-available-p sis))
    (is (climi::stream-gesture-available-p seis))
    ;; Can read characters and events.
    (is (char= #\d (stream-read-char sis)))
    (is (eql #\d (stream-read-gesture seis)))
    (is (eql lame-event (stream-read-gesture seis :peek-p t)))
    (is (eql lame-event (stream-read-gesture seis)))
    ;; Empty now, so read must time out.
    (is (null (stream-read-gesture sis :timeout 0)))
    (is (null (stream-read-gesture seis :timeout 0)))
    ;; Nothing available.
    (is (null (climi::stream-gesture-available-p sis)))
    (is (null (climi::stream-gesture-available-p seis)))))

(test stream-read-gesture.pointer-button-press-handler
  "Test `stream-read-gesture' pointer button press handler invocation."

  (let ((seis (make-instance 'standard-extended-input-stream))
        last-event)
    (labels ((handle (stream event)
               (declare (ignore stream))
               (setf last-event event))
             (check (class)
               (let ((event (make-instance
                             class :sheet          nil
                                   :modifier-state 0
                                   :button         +pointer-left-button+)))
                 (climi::stream-append-gesture seis event)
                 (stream-read-gesture seis :pointer-button-press-handler #'handle)
                 (is (eq event last-event)))))
      (check 'pointer-button-press-event)
      (check 'pointer-button-release-event)
      (check 'pointer-click-event)
      (check 'pointer-double-click-event)
      (check 'climi::pointer-scroll-event))))
