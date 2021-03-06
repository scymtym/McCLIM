;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2000 Iban Hatchondo <hatchond@emi.u-bordeaux.fr>
;;;  (c) copyright 2000 Julien Boninfante <boninfan@emi.u-bordeaux.fr>
;;;  (c) copyright 2000,2016 Robert Strandh <robert.strandh@gmail.com>
;;;  (c) copyright 2002 Gilbert Baumann <gbaumann@common-lisp.net>
;;;  (c) copyright 2017 Cyrus Harmon <cyrus@bobobeach.com>
;;;  (c) copyright 2020 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; A simple calculator.

(defpackage #:clim-demo.calculator
  (:use #:clim #:clim-lisp)
  (:export #:calculator-app))

(in-package #:clim-demo.calculator)

(defparameter *calculator-text-style*
  (make-text-style :sans-serif :roman :large))

(defun calculator ()
  (let ((frame (make-application-frame 'calculator-app)))
    (run-frame-top-level frame)
    frame))

(defun show (number)
  (setf (gadget-value (slot-value *application-frame* 'text-field))
        (princ-to-string number)))

(defun queue-number (number)
  (lambda (gadget)
    (with-slots (calc-state) (gadget-client gadget)
      (if (numberp (first calc-state))
          (setf (first calc-state) (+ (* 10 (first calc-state)) number))
          (push number calc-state))
      (show (first calc-state)))))

(defun queue-operator (operator)
  (lambda (gadget)
    (do-operation gadget)
    (with-slots (calc-state) (gadget-client gadget)
      (if (functionp (first calc-state))
          (setf (first calc-state) operator)
          (push operator calc-state)))))

(defun do-operation (gadget)
  (with-slots (calc-state) (gadget-client gadget)
    (when (= 3 (length calc-state))
      (setf calc-state (list (funcall (second calc-state) (third calc-state) (first calc-state))))
      (show (first calc-state)))))

(defun initac (gadget)
  (with-slots (calc-state) (gadget-client gadget)
    (setf calc-state (list 0)))
  (show 0))

(defun initce (gadget)
  (with-slots (calc-state) (gadget-client gadget)
    (when (numberp (first calc-state))
      (pop calc-state))
    (show 0)))

(defgeneric calculator-frame-top-level (frame &key command-parser
                                                   command-unparser
                                                   partial-command-parser
                                                   prompt)
  (:method ((frame application-frame) &key command-parser
                                           command-unparser
                                           partial-command-parser
                                           prompt)
    (declare (ignore command-parser command-unparser partial-command-parser prompt))
    (clim-extensions:simple-event-loop)))

(defun make-button (label operator)
  (make-pane 'push-button :label label
                          :activate-callback operator
                          :text-style *calculator-text-style*))

(define-application-frame calculator-app ()
  ((text-field :initform nil)
   (calc-state :initform (list 0)))
  (:menu-bar nil)
  (:panes
   (plus     (make-button "+" (queue-operator #'+)))
   (dash     (make-button "-" (queue-operator #'-)))
   (multiply (make-button "*" (queue-operator #'*)))
   (divide   (make-button "/" (queue-operator #'round)))
   (result   (make-button "=" #'do-operation))
   (one      (make-button "1" (queue-number 1)))
   (two      (make-button "2" (queue-number 2)))
   (three    (make-button "3" (queue-number 3)))
   (four     (make-button "4" (queue-number 4)))
   (five     (make-button "5" (queue-number 5)))
   (six      (make-button "6" (queue-number 6)))
   (seven    (make-button "7" (queue-number 7)))
   (eight    (make-button "8" (queue-number 8)))
   (nine     (make-button "9" (queue-number 9)))
   (zero     (make-button "0" (queue-number 0)))
   (screen   :text-field :value "0" :text-style *calculator-text-style*)
   (ac       (make-button "AC" #'initac))
   (ce       (make-button "CE" #'initce)))
  (:layouts
   (default
    (with-slots (text-field) *application-frame*
      (spacing (:thickness 1)
       (vertically (:spacing 1)
         (setf text-field screen)
         (1/6 (horizontally (:spacing 1) ac ce))
         (5/6 (tabling (:spacing 1)
                (list one two plus)
                (list three four dash)
                (list five six multiply)
                (list seven eight divide)
                (list nine zero result))))))))
  (:top-level (calculator-frame-top-level . nil)))
