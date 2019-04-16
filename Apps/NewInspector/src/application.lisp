;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Library General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Library General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Library General Public
;;;; License along with this library; if not, write to the
;;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;; Boston, MA  02111-1307  USA.

(cl:in-package #:new-inspector)

;;; Application frame

(define-application-frame inspector ()
  ;; The object slot is used once to initialize the inspector pane,
  ;; then unused.
  ((%object :initarg :object
            :reader  object))
  (:panes
   (history    history-pane)
   (inspector  inspector-pane
               :root (object *application-frame*))
   (interactor :interactor
               :min-height 100))
  (:layouts
   (default
    (vertically ()
      (scrolling (:scroll-bars :horizontal
                  :height      0)
        history)
      (:fill (scrolling () inspector))
      (make-pane 'clime:box-adjuster-gadget)
      (1/16 interactor))))
  (:command-table (application :inherit-from (inspector
                                              navigation)))
  (:command-definer nil)
  (:pointer-documentation t)
  (:default-initargs
   :width  1000
   :height 800
   :object (error "~@<Missing required initarg ~S.~@:>" :object)))

(defmethod frame-standard-output ((frame inspector))
  (find-pane-named frame 'interactor))

;;; Additional commands

(define-command (com-quit :command-table application
                          :name          t)
    ()
  (frame-exit *application-frame*))

;;; Interface

(defun inspector-name (object)
  (with-output-to-string (stream)
    (write-string "Inspector: " stream)
    (handler-case
        (with-safe-and-terse-printing (stream)
          (prin1 object stream))
      (error ()
        (write-string "<error printing object>" stream)))))

(defun inspector (object &key (new-process nil))
  (when (typep *application-frame* 'inspector)
    (restart-case (error "Clouseau called from inside Clouseau, possibly infinite recursion")
      (continue ()
        :report "Continue by starting a new Clouseau instance")
      (abort-clouseau ()
        :report "Abort this call to Clouseau"
        (return-from inspector))))

  (let ((frame (make-application-frame 'inspector :object object)))
    (flet ((run ()
             (let ((*print-length* 10)
                   (*print-level* 10))
               (run-frame-top-level frame))))
      (if new-process
          (clim-sys:make-process #'run :name (inspector-name object))
          (run))
      ; (sleep 1)
      ; (clouseau:inspector (state (find-pane-named frame 'inspector)) :new-process t)
      object)))

#+no (defvar *inspector*)
#+no (let ((timer (sb-ext:make-timer (lambda ()
                                  (queue-redisplay (clim:find-pane-named *inspector* 'inspector)))
                                :thread t)))
  (sb-ext:schedule-timer timer 1 :repeat-interval 1))
