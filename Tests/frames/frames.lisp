;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2021 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Tests for application frames.

(cl:in-package #:clim-tests)

(def-suite* :mcclim.frames
  :in :mcclim)

(defun invoke-with-frame-instance (continuation frame-class)
  (let* ((path                         '(:null))
         (climi::*default-server-path* path)
         (frame                        (make-application-frame frame-class))
         (thread                       (bt:make-thread
                                        (lambda ()
                                          (let ((climi::*default-server-path* path))
                                            (run-frame-top-level frame))))))
    ;; Slightly bad way to wait for the frame to be ready.
    (loop :until (eq (frame-state frame) :enabled))
    ;; Call the continuation.
    (funcall continuation frame)
    ;; Ask frame to exit and wait.
    (execute-frame-command frame (list 'frame-exit frame))
    (bt:join-thread thread)))

;;; Creating and destroying frames

(define-application-frame no-panes ()
  ())

(test no-panes-frame.smoke
  "Smoke test for a frame layout without any panes."
  (invoke-with-frame-instance (lambda (frame)
                                (is (eq :enabled (frame-state frame))))
                              'no-panes))

(define-application-frame single-pane ()
  ()
  (:pane :application))

(test single-pane-frame.smoke
  "Smoke test for single-pane frame layout."
  (invoke-with-frame-instance (lambda (frame)
                                (is (eq :enabled (frame-state frame))))
                              'single-pane))

;;; Enabling and disabling

(define-application-frame enable-disable-frame ()
  ((%events :accessor events
            :initform '()))
  (:pane :application))

(defmethod note-frame-enabled ((frame-manager frame-manager)
                               (frame         enable-disable-frame))
  (push :enable (events frame)))

(defmethod note-frame-disabled ((frame-manager frame-manager)
                                (frame         enable-disable-frame))
  (push :disable (events frame)))

(test enable-disable-frame.smoke
  "Smoke test for enabling and disabling frames."
  (invoke-with-frame-instance
   (lambda (frame)
     ;; Clear event buffer.
     (loop :until (equal (events frame) '(:enable)))
     (setf (events frame) '())
     ;; Disable.
     (disable-frame frame)
     (is (equal '(:disable) (reverse (events frame))))
     ;; Enable again.
     (enable-frame frame)
     (is (equal '(:disable :enable) (reverse (events frame)))))
   'enable-disable-frame))
