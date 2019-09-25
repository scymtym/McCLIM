;;;;  Copyright (c) 2019 Jan Moringen
;;;;
;;;;    License: BSD-2-Clause.

(in-package #:clim-demo)

;;; Sheet stack tracking and boundary event count tracking

(defvar *sheet-stacks*
  (make-hash-table :test #'eq))

(defun sheet-stack (sheet)
  (let ((port (port sheet)))
    (cdr (alexandria:ensure-gethash port *sheet-stacks* (cons nil nil)))))

(defun (setf sheet-stack) (new-value sheet)
  (let ((port (port sheet)))
    (setf (cdr (alexandria:ensure-gethash port *sheet-stacks* (cons nil nil))) new-value)))

(defvar *sheet-stack-inspector*
  (nth-value 1 (clouseau:inspect *sheet-stacks* :new-process t)))

(defun verify-sheet-stack (sheet)
  (when (boundp '*sheet-stack-inspector*)
    (setf (clouseau:root-object *sheet-stack-inspector* :run-hook-p t)
          *sheet-stacks*))

  (loop for (first next) on (sheet-stack sheet)
        for parent = (sheet-parent first)
        do (if next
               (assert (eq parent next))
               (assert (or (null parent) ; sheet has been degrafted?
                           (climi::graftp parent))))))

(defun push-enter (sheet)
  (push sheet (sheet-stack sheet))
  (verify-sheet-stack sheet))

(defun pop-exit (sheet)
  (assert (eq sheet (first (sheet-stack sheet))))
  (pop (sheet-stack sheet))
  (verify-sheet-stack sheet))

(defvar *boundary-counts* (make-hash-table :test #'eq))

(defun count-enter (sheet)
  (let ((count (incf (gethash sheet *boundary-counts* 0))))
    (assert (<= 0 count 1))))

(defun count-exit (sheet)
  (let ((count (decf (gethash sheet *boundary-counts* 0))))
    (assert (<= 0 count 1))))

(defmethod dispatch-event :around ((sheet basic-pane) (event pointer-enter-event))
  (push-enter sheet)
  (count-enter sheet)
  (call-next-method)
  ; (handle-repaint sheet +everywhere+)
  )

(defmethod dispatch-event :around ((sheet basic-pane) (event pointer-exit-event))
  (pop-exit sheet)
  (count-exit sheet)
  (call-next-method)
  ; (handle-repaint sheet +everywhere+)
  )

;;; Visualization

(defun sheet-background (sheet)
  (let ((count (gethash sheet *boundary-counts*))
        (position (position sheet (sheet-stack sheet))))
    (cond ((and count (< count 0))
           +red+)
          ((and count (> count 1))
           +green+)
          (position
           (let ((i (- (length (sheet-stack sheet)) position)))
             (make-contrasting-inks 8 (mod i 8)))))))

(defmethod climi::pane-background :around ((pane basic-pane))
  (or (sheet-background pane) (call-next-method)))
