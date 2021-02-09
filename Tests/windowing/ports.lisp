;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2021 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Tests for the core port functionality.

(in-package #:clim-tests)

(def-suite* :mcclim.windowing.ports
  :in :mcclim)

(defclass mock-click-events-port (climi::click-events-mixin)
  ((%events :accessor events
            :initform '())))

(defmethod distribute-event ((port mock-click-events-port) (event t))
  (push event (events port)))

(test click-events-mixin.smoke
  "Smoke test for the `click-events-mixin' class."

  (let ((port (make-instance 'mock-click-events-port)))
    (flet ((event (class button)
             (let ((event (make-instance class :sheet nil
                                               :modifier-state 0
                                               :x 0 :y 0
                                               :button button)))
               (distribute-event port event))))
      ;; Left click
      (event 'pointer-button-press-event +pointer-left-button+)
      (is (eql 1 (length (events port))))
      (event 'pointer-button-release-event +pointer-left-button+)
      (is (eql 3 (length (events port))))
      (let ((event (first (events port))))
        (is-true (typep event 'pointer-click-event))
        (is (eql +pointer-left-button+ (pointer-event-button event))))
      ;; Left double click
      (event 'pointer-button-press-event +pointer-left-button+)
      (is (eql 4 (length (events port))))
      (event 'pointer-button-release-event +pointer-left-button+)
      (is (eql 7 (length (events port))))
      (let ((event (first (events port))))
        (is-true (typep event 'pointer-double-click-event))
        (is (eql +pointer-left-button+ (pointer-event-button event))))
      ;; Right button click
      (event 'pointer-button-press-event +pointer-right-button+)
      (is (eql 8 (length (events port))))
      (event 'pointer-button-release-event +pointer-right-button+)
      (is (eql 10 (length (events port))))
      (let ((event (first (events port))))
        (is-true (typep event 'pointer-click-event))
        (is (eql +pointer-right-button+ (pointer-event-button event)))))))
