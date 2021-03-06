;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2004 Peter Mechlenborg <metch@daimi.au.dk>
;;;  (c) copyright 2016,2017 Daniel Kochmański <daniel@turtleware.eu>
;;;  (c) copyright 2017,2021 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; This is the beginning of a Common Lisp debugger implemented in
;;; McCLIM. It uses the portable debugger interface developed for the
;;; Slime project, and the graphical layout is also heavily inspired
;;; by Slime. Because of Slime I hope that this works on other
;;; implementations than SBCL.
;;;
;;; Test:
;;;
;;; For at quick test, you can use this code snippet:
;;;
;;; (clim-debugger:with-debugger ()
;;;   (+ 3 'abc))
;;;
;;; This is also nice :-)
;;;
;;; (clim-debugger:with-debugger ()
;;;   (clim-listener:run-listener :new-process t))
;;;
;;; Problems/todo:
;;;
;;; - Goto source location is not supported, but I think this could be
;;;   done through slime.
;;;
;;; - Frames could be navigable with arrow keys as well. How to do that?

(defpackage #:clim-debugger
  (:use #:clim #:clim-lisp #:clim-extensions)

  (:import-from #:alexandria
   #:when-let)

  (:export #:debugger #:with-debugger #:install-debugger))

(in-package :clim-debugger)

;;; Data model

(defclass debugger-info ()
  ((the-condition :accessor the-condition
                  :initarg :the-condition)
   (condition-message :accessor condition-message
                      :initarg  :condition-message)
   (type-of-condition :accessor type-of-condition
                      :initarg  :type-of-condition)
   (condition-extra :accessor condition-extra
                    :initarg  :condition-extra)
   (restarts :accessor restarts
             :initarg :restarts)
   (backtrace :accessor backtrace
              :initarg :backtrace)))

(defclass minimized-stack-frame-view (textual-view)())
(defclass maximized-stack-frame-view (textual-view)())

(defparameter +minimized-stack-frame-view+
  (make-instance 'minimized-stack-frame-view))
(defparameter +maximized-stack-frame-view+
  (make-instance 'maximized-stack-frame-view))

(defclass stack-frame ()
  ((clim-view       :accessor view :initform +minimized-stack-frame-view+)
   (frame-string    :accessor frame-string
                    :initarg  :frame-string)
   (frame-no        :accessor frame-no
                    :initarg :frame-no)
   (frame-variables :accessor frame-variables
                    :initarg :frame-variables)))

(defun compute-backtrace (start end)
  (loop for frame    in   (swank-backend::compute-backtrace start end)
        for frame-no from 0
        collect (make-instance
                 'stack-frame
                 :frame-string    (let ((*print-pretty* nil))
                                    (with-output-to-string (stream)
                                      (swank-backend::print-frame frame stream)))
                 :frame-no        frame-no
                 :frame-variables (swank-backend::frame-locals frame-no))))

;;; CLIM stuff

(defclass debugger-pane (clouseau:inspector-pane)
  ((condition-info :accessor condition-info :initarg :condition-info)
   (active-frame :accessor active-frame :initform 0)
   (shown-frames :accessor shown-frames :initform 20)))

(flet ((maybe-more (sheet)
         (when (and (< (abs (- (bounding-rectangle-max-y (pane-viewport-region sheet))
                               (bounding-rectangle-max-y (sheet-region sheet))))
                       1)
                    (< (shown-frames sheet)
                       (length (backtrace (condition-info sheet)))))
           (let ((frame (pane-frame sheet)))
             (execute-frame-command frame '(com-more))
             (redisplay-frame-pane frame sheet)))))

  (defmethod note-sheet-region-changed ((sheet debugger-pane))
    (maybe-more sheet))

  (defmethod note-sheet-transformation-changed ((sheet debugger-pane))
    (maybe-more sheet)))

(define-application-frame clim-debugger ()
  ((condition        :initform nil :accessor the-condition)
   (returned-restart :initform nil :accessor returned-restart))
  (:pointer-documentation t)
  (:panes (debugger-pane (let ((condition (the-condition *application-frame*)))
                           (make-pane 'debugger-pane
                                      :root condition
                                      :condition-info condition
                                      :end-of-line-action :allow
                                      :end-of-page-action :scroll)))
          (interactor    :interactor :min-height 100 :height 100))
  (:layouts
   (without-interactor
     (scrolling (:height 480 :width #.(* 480 slim:+golden-ratio+))
       debugger-pane))
   (with-interactor
     (vertically ()
       (:fill (scrolling (:height 380 :width #.(* 480 slim:+golden-ratio+))
                debugger-pane))
       (make-pane 'clime:box-adjuster-gadget)
       interactor)))
  (:geometry :height 480 :width #.(* 480 slim:+golden-ratio+))
  (:command-table (clim-debugger :inherit-from (clouseau:inspector-command-table))))

(defmethod (setf the-condition) :after ((new-value t) (object clim-debugger))
  (when-let ((pane (find-pane-named object 'debugger-pane)))
    (setf (condition-info pane) new-value)))

(defmethod frame-standard-output ((frame clim-debugger))
  (or (find-pane-named frame 'interactor)
      (call-next-method)))

;;; Presentation types

(define-presentation-type stack-frame () :inherit-from 't)
(define-presentation-type restart     ())
(define-presentation-type inspectable ())

;;; Gestures

(define-gesture-name :prev    :keyboard (#\p :meta))
(define-gesture-name :next    :keyboard (#\n :meta))
(define-gesture-name :exit    :keyboard (#\q))
(define-gesture-name :eval    :keyboard (#\e))
(define-gesture-name :toggle  :keyboard #\tab)

;;; Restart keyboard shortcuts

(macrolet ((invoke-x (x)
             (let* ((char (aref (format nil "~A" x) 0))
                    (name (alexandria:symbolicate "INVOKE-RESTART-" char)))
               `(progn
                  (define-clim-debugger-command (,name :keystroke (,char)) ()
                    (let* ((pane (find-pane-named
                                  *application-frame* 'debugger-pane))
                           (restart (nth ,x (restarts (condition-info pane)))))
                      (when restart
                        (com-invoke-restart restart))))))))
  (invoke-x 0) (invoke-x 1) (invoke-x 2) (invoke-x 3) (invoke-x 4)
  (invoke-x 5) (invoke-x 6) (invoke-x 7) (invoke-x 8) (invoke-x 9))

;;; Commands

(define-clim-debugger-command (com-more) ()
  (let ((pane (find-pane-named *application-frame* 'debugger-pane)))
    (setf #1=(shown-frames pane)
          (min (+ #1# 10) (length (backtrace (condition-info pane)))))))

(define-clim-debugger-command (com-invoke-inspector :name "Inspect in new frame")
    ((obj inspectable :gesture (:select
                                :documentation "Inspect in new frame"
                                :pointer-documentation "Inspect in new frame")))
  (clouseau:inspect obj :new-process t))

(define-clim-debugger-command (com-refresh :name "Refresh" :menu t
                                           :keystroke #\r)
    ()
  (change-space-requirements (frame-panes *application-frame*)))

(define-clim-debugger-command (com-next :keystroke :next)
    ()
  (let* ((pane (find-pane-named *application-frame* 'debugger-pane))
         (shown-frames (shown-frames pane)))
    (incf (active-frame pane))
    (when (= (active-frame pane) shown-frames)
      (com-more))
    (when (= (active-frame pane) shown-frames)
      (decf (active-frame pane)))))

(define-clim-debugger-command (com-prev :keystroke :prev)
    ()
  (let ((pane (find-pane-named *application-frame* 'debugger-pane)))
    (setf #1=(active-frame pane) (max (1- #1#) 0))))

(define-clim-debugger-command (com-eval :name "Eval in frame" :menu t
                                        :keystroke :eval)
    ((form clim:string))
  (let* ((dbg-pane (find-pane-named *application-frame* 'debugger-pane))
         (active-frame (active-frame dbg-pane)))
    (format *pointer-documentation-output*
            (swank:eval-string-in-frame
             form active-frame (swank-backend:frame-package active-frame)))))

(define-clim-debugger-command (com-quit :name "Quit" :menu t
                                        :keystroke :exit) ()
  (frame-exit *application-frame*))

(define-clim-debugger-command (com-invoke-restart :name "Invoke restart")
    ((restart 'restart :gesture :select))
  (setf (returned-restart *application-frame*) restart)
  (frame-exit *application-frame*))

(define-clim-debugger-command (com-toggle-stack-frame-view
                               :name "Toggle stack frame view")
    ((stack-frame 'stack-frame :gesture (:select :documentation "Toggle stack frame view")))

  (let ((dbg-pane (find-pane-named *application-frame* 'debugger-pane)))
    (setf (active-frame dbg-pane) (frame-no stack-frame)))

  (if (eq +minimized-stack-frame-view+ (view stack-frame))
      (setf (view stack-frame) +maximized-stack-frame-view+)
      (setf (view stack-frame) +minimized-stack-frame-view+))
  (change-space-requirements (frame-panes *application-frame*)))

(define-clim-debugger-command (com-toggle-active-frame-view
                               :keystroke :toggle
                               :name "Toggle active")
    ()
  (let ((dbg-pane (find-pane-named *application-frame* 'debugger-pane)))
    (com-toggle-stack-frame-view
     (nth (active-frame dbg-pane) (backtrace (condition-info dbg-pane))))))

(define-clim-debugger-command (clim-toggle-interactor
                               :name      "Toggle interactor"
                               :keystroke (#\i :control))
    ()
  (let ((frame *application-frame*))
    (setf (frame-current-layout frame)
          (case (frame-current-layout frame)
            (without-interactor 'with-interactor)
            (with-interactor    'without-interactor)))))

;;; Display debugging info

(defmethod redisplay-frame-pane ((frame application-frame)
                                 (pane debugger-pane)
                                 &key force-p)
  (declare (ignore force-p))
  (clouseau:call-with-root-place
   (lambda ()
     (when-let ((info (condition-info pane))
                (thread (bt:current-thread)))
       (formatting-table (pane)
         (formatting-row (pane)
           (formatting-cell (pane)
             (with-text-face (pane :bold) (write-string "Description" pane)))
           (formatting-cell (pane)
             (princ (condition-message info) pane)))
         (with-output-as-presentation
             (pane thread 'inspectable :single-box t)
           (formatting-row (pane)
             (formatting-cell (pane)
               (with-text-face (pane :bold) (write-string "Condition" pane)))
             (formatting-cell (pane)
               (with-drawing-options (pane :ink +red+)
                 (clouseau:formatting-place
                     (nil 'clouseau:pseudo-place (the-condition info) nil present-value)
                   (present-value pane))))))
         (when-let ((extra (condition-extra info)))
           (formatting-row (pane)
             (formatting-cell (pane)
               (with-text-face (pane :bold) (write-string "Extra" pane)))
             (formatting-cell (pane)
               (with-text-family (pane :fix)
                 (princ extra pane)))))
         (with-output-as-presentation
             (pane thread 'inspectable :single-box t)
           (formatting-row (pane)
             (formatting-cell (pane)
               (with-text-face (pane :bold) (write-string "Thread" pane)))
             (formatting-cell (pane)
               (clouseau:formatting-place
                   (nil 'clouseau:pseudo-place thread nil present-value)
                 (present-value pane))))))
       (fresh-line pane)

       (with-text-face (pane :bold) (write-string "Restarts" pane))
       (fresh-line pane)
       (indenting-output (pane ">")
         (formatting-table (pane :x-spacing 10)
           (do* ((restarts (restarts info) (cdr restarts))
                 (r #1=(car restarts) #1#)
                 (n 0 (1+ n)))
                ((null restarts) t)
             (with-output-as-presentation (pane r 'restart :single-box t)
               (formatting-row (pane)
                 (formatting-cell (pane :align-y :top)
                   (climi::format-keyboard-gesture
                    `(:keyboard ,(digit-char n) 0) :stream pane))
                 (formatting-cell (pane)
                   (with-drawing-options (pane :ink +dark-violet+)
                     (princ (restart-name r) pane)))
                 (formatting-cell (pane) (princ r pane)))))))
       (fresh-line pane)
       (display-backtrace frame pane)

       (let ((history (stream-output-history pane)))
         (change-space-requirements
          pane :width (bounding-rectangle-width history)
          :height (bounding-rectangle-height history)))))
   (clouseau:root-place pane) pane))

(defun display-backtrace (frame pane)
  (declare (ignore frame))
  (with-text-face (pane :bold)
    (write-string "Backtrace:" pane))
  (fresh-line pane)
  (indenting-output (pane ">")
    (formatting-table (pane)
      (do* ((back (backtrace (condition-info pane)) (cdr back))
            (stack-frame #1=(car back) #1#))
           ((or (null back)
                (= (frame-no stack-frame)
                   (shown-frames pane))))
        (with-output-as-presentation
            (pane stack-frame 'stack-frame :single-box t)
          (formatting-row (pane)
            (formatting-cell (pane :align-x :right)
              (with-drawing-options (pane :ink +gray41+)
                (prin1 (frame-no stack-frame) pane)))
            (formatting-cell (pane)
              (with-drawing-options (pane :ink (if (= (frame-no stack-frame)
                                                      (active-frame pane))
                                                   +red4+ +blue4+))
                (present stack-frame 'stack-frame
                         :stream pane :view (view stack-frame) :single-box t)))))))))

(defun print-stack-frame-header (object stream)
  (let* ((frame-string (frame-string object))
         (new-line-pos (position #\newline frame-string)))
    (if new-line-pos
        (format stream "~A ..)" (subseq frame-string 0 new-line-pos))
        (princ frame-string stream))))

(define-presentation-method present (object (type stack-frame) stream
                                            (view minimized-stack-frame-view)
                                            &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (print-stack-frame-header object stream))

(defclass frame-local-place (clouseau:pseudo-place)
  ())

(defmethod clouseau:value ((place frame-local-place))
  (getf (clouseau:cell place) :value))

(define-presentation-method present (object (type stack-frame) stream
                                            (view maximized-stack-frame-view)
                                            &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (print-stack-frame-header object stream)
  (fresh-line stream)
  (if (null (frame-variables object))
      (write-string "  No locals." stream)
      (progn
        (with-text-face (stream :bold) (write-string "Locals" stream))
        (fresh-line stream)
        (indenting-output (stream ">")
          (formatting-table (stream)
            (loop for info in (frame-variables object)
                  for name = (getf info :name)
                  for value = (getf info :value)
                  do (with-output-as-presentation
                         (stream value 'inspectable :single-box t)
                       (clouseau:format-place-row
                        stream object 'frame-local-place info :label name)))))))
  (fresh-line stream))

(define-presentation-method present (object (type restart) stream
                                            (view textual-view)
                                            &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (with-text-face (stream :bold) (princ (restart-name object) stream)))

(define-presentation-method present (object (type inspectable) stream
                                            (view textual-view)
                                            &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (princ object stream))

;;; Starting the debugger

(defun run-debugger-frame ()
  (run-frame-top-level
   (make-application-frame 'clim-debugger)))

(defun debugger (condition me-or-my-encapsulation)
  (let ((debugger-frame (make-application-frame 'clim-debugger)))
    (swank-backend::call-with-debugging-environment
     (lambda ()
       (unwind-protect
            (setf (the-condition debugger-frame)
                  (make-instance
                   'debugger-info
                   :the-condition        condition
                   :type-of-condition    (type-of condition)
                   :condition-message    (swank::safe-condition-message condition)
                   :condition-extra      (swank::condition-extras       condition)
                   :restarts             (compute-restarts)
                   :backtrace (compute-backtrace 0 nil)))
         (run-frame-top-level debugger-frame)
         (let ((restart (returned-restart debugger-frame)))
           (if restart
               (let ((*debugger-hook* me-or-my-encapsulation))
                 (invoke-restart-interactively restart))
               (abort))))))))

(defvar *debugger-bindings*
  `((*debugger-hook*                      . #'debugger)
    #+abcl (sys::*invoke-debugger-hook*   . #'debugger)
    #+ccl  (ccl:*break-hook*              . #'debugger)
    #+ecl  (ext:*invoke-debugger-hook*    . #'debugger)
    #+sbcl (sb-ext:*invoke-debugger-hook* . #'debugger)
    (bt:*default-special-bindings* . *debugger-bindings*)
    ,@bt:*default-special-bindings*))

(defmacro with-debugger (options &body body)
  (assert (null options) nil "Options should be empty.")
  `(let ((bt:*default-special-bindings* *debugger-bindings*)
         (*debugger-hook* #'debugger)
         #+abcl (sys::*invoke-debugger-hook* #'debugger)
         #+ccl (ccl:*break-hook* #'debugger)
         #+ecl (ext:*invoke-debugger-hook* #'debugger)
         #+sbcl (sb-ext:*invoke-debugger-hook* #'debugger))
     ,@body))

(defun install-debugger ()
  (setf *debugger-hook* #'debugger)
  #+abcl (setf sys::*invoke-debugger-hook*   #'debugger)
  #+ccl  (setf ccl:*break-hook*              #'debugger)
  #+ecl  (setf ext:*invoke-debugger-hook*    #'debugger)
  #+sbcl (setf sb-ext:*invoke-debugger-hook* #'debugger))

;;; For testing

(defun simple-break ()
  (with-simple-restart  (continue "Continue from interrupt.")
    (with-debugger ()
      (invoke-debugger
       (make-condition 'simple-error
                       :format-control "Debugger test")))))
