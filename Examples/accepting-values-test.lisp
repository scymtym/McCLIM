;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2017 Alessandro Serra <gas2serra@gmail.com>
;;;  (c) copyright 2018 admich <andrea.demichele@gmail.com>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Several examples of using `accepting-values'.

(cl:in-package #:clim-demo.accepting-values)

(define-application-frame av-test ()
  ((own-window-p :initform nil))

  (:panes
   (screen :application
           :display-time t
           :display-function #'av-test-display-screen
           :text-style (make-text-style :sans-serif :roman :normal))
   (own-window-option
    (with-radio-box (:orientation :vertical
                     :value-changed-callback
                     #'(lambda (gadget value)
                         (with-slots (own-window-p) (gadget-client gadget)
                           (setf own-window-p
                                 (string= (gadget-label value) "yes")))))
      (radio-box-current-selection "no")
      "yes"))
   (interactor :interactor :min-width 600))
  (:menu-bar t)
  (:pointer-documentation t)
  (:layouts
   (defaults
    (horizontally ()
      (vertically ()
        (labelling (:label "Own Window")
          own-window-option)
        +fill+)
      (vertically ()
        screen
        interactor)))))

(defvar *tests* '())

(defun av-test-display-screen (frame pane)
  (declare (ignore frame))
  (with-text-size (pane :large)
    (flet ((present-test (command)
             (fresh-line pane)
             (present `(,command) 'command :stream pane)))
      (map nil #'present-test (reverse *tests*)))
    (fresh-line pane)))

(define-av-test-command (com-refresh-av-test :name t :menu t)
    ()
  (let* ((frame *application-frame*)
         (screen (find-pane-named frame 'screen)))
    (window-clear screen)
    (window-clear (find-pane-named frame 'interactor))
    (av-test-display-screen frame screen)))

(macrolet ((def (command-name test-name &key (own-window-p t) (arguments '()))
             `(progn
                (pushnew ',command-name *tests*)
                (define-av-test-command (,command-name :name t :menu nil)
                    ()
                  ,(if own-window-p
                       `(with-slots (own-window-p) *application-frame*
                          (format t "Result: ~S~%" (multiple-value-list
                                                    (,test-name :ow own-window-p))))
                       `(format t "Result: ~S~%" (multiple-value-list
                                                  (,test-name ,@arguments))))
                  (finish-output *standard-output*)))))
  (def com-accepting-interval            accepting-interval)
  (def com-accepting-square              accepting-square)
  (def com-reset-clock-1                 reset-clock-1)
  (def com-reset-clock-2                 reset-clock-2)
  (def com-accepting-tag                 accepting-tag)
  (def com-menu-choose-1                 menu-choose-1                 :own-window-p nil)
  (def com-menu-choose-2                 menu-choose-2                 :own-window-p nil)
  (def com-menu-choose-3                 menu-choose-3                 :own-window-p nil)
  (def com-menu-choose-4                 menu-choose-4                 :own-window-p nil)
  (def com-accept-popup                  accept-popup                  :own-window-p nil
                                                                       :arguments ('(1 2 3 4 5 6 7 8)))
  (def com-accepting-with-list-pane-view accepting-with-list-pane-view)
  (def com-accepting-with-gadgets        accepting-with-gadgets))
