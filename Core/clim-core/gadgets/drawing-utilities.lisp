;;;  Drawing Utilities for Concrete Gadgets

(in-package #:climi)

(defun display-gadget-background (gadget color x1 y1 x2 y2)
  (draw-rectangle* gadget x1 y1 x2 y2 :ink color :filled t))

;;; Engraved text

(defun draw-label-text* (stream text x y &key ink engravedp)
  (cond (engravedp
         (draw-text* stream text (1+ x) (1+ y) :ink *3d-light-color*)
         (draw-text* stream text x      y      :ink *3d-dark-color*))
        (t
         (draw-text* stream text x y :ink ink))))

;;; Labels

(defgeneric compose-label-space (gadget &key wider higher))

(defgeneric draw-label* (gadget x1 y1 x2 y2 &key label ink activep))

(defmethod compose-label-space ((gadget labelled-gadget-mixin) &key (wider  0)
                                                                    (higher 0))
  (with-slots (align-x align-y label) gadget
    (let* ((text-style (pane-text-style gadget))
           (as (text-style-ascent text-style gadget))
           (ds (text-style-descent text-style gadget))
           (w  (+ (text-size gadget label :text-style text-style) wider))
           (h  (+ as ds higher)))
      (make-space-requirement :width w  :min-width w  :max-width  +fill+
                              :height h :min-height h :max-height +fill+))))

(defmethod draw-label* ((gadget gadget) x1 y1 x2 y2
                        &key (label   (alexandria:required-argument :label))
                             (align-x (pane-align-x gadget))
                             (align-y (pane-align-y gadget))
                             (ink     +foreground-ink+)
                             (activep (gadget-active-p gadget)))
  (let ((text-style (pane-text-style gadget)))
    (flet ((width () (text-size gadget label :text-style text-style))
           (as () (text-style-ascent text-style gadget))
           (ds () (text-style-descent text-style gadget)))
      (let ((x (ecase align-x
                 (:left   x1)
                 (:right  (- x2 (width)))
                 (:center (/ (+ x1 x2 (- (width))) 2))))
            (y (ecase align-y
                 (:top      (+ y1 (as)))
                 (:center   (/ (+ y1 y2 (- (as) (ds))) 2))
                 (:bottom   (- y2 (ds)))
                 (:baseline y2))))
        (draw-label-text* gadget label x y :ink ink :engravedp (not activep))))))

(defmethod draw-label* ((gadget labelled-gadget-mixin) x1 y1 x2 y2
                        &rest args
                        &key (label (gadget-label gadget)) ink activep)
  (declare (ignore ink activep))
  (apply #'call-next-method gadget x1 y1 x2 y2 :label label
         (alexandria:remove-from-plist args :label)))
