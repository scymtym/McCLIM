(in-package #:clim-internals)

;;; Arming and disarming gadgets

(defgeneric arm-gadget (gadget &optional value))

(defgeneric disarm-gadget (gadget))

(defgeneric armed-callback (gadget client gadget-id)
  (:argument-precedence-order client gadget-id gadget))

(defgeneric disarmed-callback (gadget client gadget-id)
  (:argument-precedence-order client gadget-id gadget))

;;; Activation

(defgeneric activate-gadget (gadget))
(defgeneric deactivate-gadget (gadget))
(defgeneric note-gadget-activated (client gadget))
(defgeneric note-gadget-deactivated (client gadget))

;;; Value gadget

;; TODO in decls (defgeneric gadget-value (gadget)

;; TODO in decls (defgeneric (setf gadget-value))

(defgeneric value-changed-callback (gadget client gadget-id value)
  (:argument-precedence-order client gadget-id value gadget))

;;; Range gadget

(defgeneric gadget-range (range-gadget)
  (:documentation
   "Returns the difference of the maximum and minimum value of RANGE-GADGET."))

(defgeneric gadget-range* (range-gadget)
  (:documentation
   "Returns the minimum and maximum value of RANGE-GADGET as two values."))

;;; 30.4.4 The abstract scroll-bar Gadget

(defgeneric drag-callback (pane client gadget-id value)
  (:argument-precedence-order client gadget-id value pane))

(defgeneric scroll-to-top-callback (scroll-bar client gadget-id)
  (:argument-precedence-order client gadget-id scroll-bar))

(defgeneric scroll-to-bottom-callback (scroll-bar client gadget-id)
  (:argument-precedence-order client gadget-id scroll-bar))

(defgeneric scroll-up-line-callback (scroll-bar client gadget-id)
  (:argument-precedence-order client gadget-id scroll-bar))

(defgeneric scroll-up-page-callback (scroll-bar client gadget-id)
  (:argument-precedence-order client gadget-id scroll-bar))

(defgeneric scroll-down-line-callback (scroll-bar client gadget-id)
  (:argument-precedence-order client gadget-id scroll-bar))

(defgeneric scroll-down-page-callback (scroll-bar client gadget-id)
  (:argument-precedence-order client gadget-id scroll-bar))

;;; 30.4.6 The abstract radio-box and check-box Gadgets

(defgeneric radio-box-current-selection (radio-box))

(defgeneric (setf radio-box-current-selection) (new-value radio-box))

(defgeneric radio-box-selections (radio-box))

;;;; CHECK-BOX

(defgeneric check-box-current-selection (check-box))

(defgeneric (setf check-box-current-selection) (new-value check-box))

;;; Labels

(defgeneric compose-label-space (gadget &key wider higher))

(defgeneric draw-label* (pane x1 y1 x2 y2 &key ink))

;;; 3D Border

(defgeneric pane-inner-region (pane))

;;; Common colors:

(defgeneric gadget-highlight-background (gadget))

(defgeneric effective-gadget-foreground (gadget))

(defgeneric effective-gadget-background (gadget))

(defgeneric effective-gadget-input-area-color (gadget))

;;; Scroll bar

(defgeneric scroll-bar-transformation (scroll-bar))

(defgeneric scroll-bar-up-region (scroll-bar))

(defgeneric scroll-bar-down-region (scroll-bar))

(defgeneric scroll-bar-thumb-bed-region (scroll-bar-pane))

(defgeneric scroll-bar-thumb-region (scroll-bar-pane &optional value))

;;; The concrete slider Gadget

(defgeneric convert-position-to-value (slider-pane position)
  (:documentation
   "Return gadget value for SLIDER-PANE corresponding to POSITION.

    POSITION can be a real number or a pointer event. Both designate a
    horizontal or vertical position in the gadget's coordinate
    system."))

(defgeneric convert-value-to-position (slider-pane)
  (:documentation
   "Return a position for SLIDER-PANE's gadget value.

    The returned position measures a distance along the horizontal or
    vertical axis of the gadget's coordinate system."))

;;; The concrete list-pane and option-pane Gadgets

(defgeneric generic-list-pane-item-strings (generic-list-pane))

(defgeneric generic-list-pane-item-values (generic-list-pane))

(defgeneric generic-list-pane-items-width (generic-list-pane))

(defgeneric generic-list-pane-items-length (generic-list-pane))

(defgeneric generic-list-pane-item-height (generic-list-pane))

(defgeneric (setf list-pane-items)
    (newval pane &key invoke-callback)
  (:documentation
   "Set the current list of items for this list pane.
The current GADGET-VALUE will be adjusted by removing values not
specified by the new items.  VALUE-CHANGED-CALLBACK will be called
if INVOKE-CALLBACK is given."))

;;; OPTION-PANE

(defgeneric generic-option-pane-widget-size (pane))

(defgeneric generic-option-pane-draw-widget (pane))

;;; Orientation

(defgeneric orientation (gadget))
