(in-package #:clim-internals)

(defclass theme ()
  ((%properties :reader   %properties
                :initform (make-hash-table :test #'eq))))

(defmethod property-value ((property t) (thing theme))
  (gethash property (%properties thing)))

(defmethod (setf property-value) ((new-value t) (property t) (thing theme))
  (setf (gethash property (%properties thing)) new-value))

(defmethod (setf property-value) :around ((new-value t) (property t) (thing theme))
  (let* ((old-value      (property-value property thing))
         (old-properties (list (cons property old-value))))
    (call-next-method)
    (map-over-frames
     (lambda (frame)
       (map-over-sheets
        (lambda (sheet)
          (when (typep sheet 'animated-mixin)
            (add-animation (make-instance 'changing-theme
                                          :gadget         sheet
                                          :old-properties old-properties)
                           sheet)))
        (frame-top-level-sheet frame))))))

(defclass changing-theme (animation)
  ((%old-properties :initarg :old-properties
                    :reader  %old-properties)))

(defmethod property-value ((property t) (thing changing-theme))
  (let ((theme-value (property-value property *theme*)))
    (when-let ((old-cell (assoc property (%old-properties thing))))
      (let ((old-value (cdr old-cell))
            (p (progress thing)))
        (values
         (etypecase old-value
           (real
            (alexandria:lerp p old-value theme-value))
           ((or color ink)
            (compose-over (compose-in theme-value (make-opacity p))
                          old-value)))
         t)))))

;;;

;; gadget-color-mixin
;;   normal                 -> +gray80+
;;   highlighted            -> +gray85+
;;   pushed-and-highlighted -> +gray75+

;; effective-gadget-*
;;   highlight-background   -> (over (in white .5) pane-background)
;;   foreground[active]     -> +foreground-ink+
;;   foreground[not-active  -> (over (in pane-foreground .5) pane-background)
;;   background[armed]      -> highlight-background
;;   background[not-armed]  -> pane-background
;;   input-area[active]     -> +lemonchiffon+
;;   input-area[not-active] -> (over (in +lemonchiffon+ .5) pane-background)

;; 3d colors
;;  *3d-dark-color*   -> (make-gray-color .59)
;;  *3d-normal-color* -> (make-gray-color .84)
;;  *3d-light-color*  -> (make-gray-color 1.0)
;;  *3d-inner-color*  -> (make-gray-color .75)

;;; TODO indirect ink for theme-foreground, theme-background, etc.?

(defun default-theme ()
  (let ((theme (make-instance 'theme)))
    (setf (property-value :foreground-ink           theme) +foreground-ink+
          (property-value :background-ink           theme) +gray84+
          (property-value :highlight-background-ink theme) (compose-over (compose-in +white+ (make-opacity .5))
                                                                         +gray84+)

          (property-value :3d-light-color           theme) (make-gray-color 1.0)
          (property-value :3d-dark-color            theme) (make-gray-color .59))
    theme))

(defvar *theme* (default-theme))

(setf (property-value :background-ink           *theme*) +purple+
      (property-value :foreground-ink           *theme*) +white+
      (property-value :highlight-background-ink *theme*) +green+

      (property-value :3d-dark-color            *theme*) +orange+
      (property-value :3d-normal-color          *theme*) +dark-blue+
      (property-value :3d-light-color           *theme*) +yellow+
      (property-value :3d-inner-color           *theme*) +orange3+)

(setf (property-value :background-ink           *theme*) +gray30+
      (property-value :foreground-ink           *theme*) +gray85+
      (property-value :highlight-background-ink *theme*) +gray45+

      (property-value :3d-dark-color            *theme*) +gray30+
      (property-value :3d-normal-color          *theme*) +gray45+
      (property-value :3d-light-color           *theme*) +gray85+
      (property-value :3d-inner-color           *theme*) +gray35+)

(setf (property-value :background-ink           *theme*) +gray84+
      (property-value :foreground-ink           *theme*) +black+
      (property-value :highlight-background-ink *theme*) (compose-over (compose-in +white+ (make-opacity .5))
                                                                       +gray84+)

      (property-value :3d-dark-color            *theme*) (make-gray-color .59)
      (property-value :3d-normal-color          *theme*) (make-gray-color .84)
      (property-value :3d-light-color           *theme*) (make-gray-color 1.0)
      (property-value :3d-inner-color           *theme*) (make-gray-color .75))
