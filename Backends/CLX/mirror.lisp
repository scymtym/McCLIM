(in-package :clim-clx)

;;; Return a string in which every non-STANDARD-CHAR in STRING has
;;; been replaced with #\_. The result is guaranteed to be an ASCII
;;; string.
(defun %ensure-standard-characters (string)
  (substitute-if-not #\_ (alexandria:of-type 'standard-char) string))

(defun %set-window-name (window name)
  (setf (xlib:wm-name window) (%ensure-standard-characters name))
  (xlib:change-property window
                        :_NET_WM_NAME
                        (babel:string-to-octets name :encoding :utf-8)
                        :UTF8_STRING 8))

(defun %set-window-icon-name (window icon-name)
  (setf (xlib:wm-icon-name window) (%ensure-standard-characters icon-name))
  (xlib:change-property window
                        :_NET_WM_ICON_NAME
                        (babel:string-to-octets icon-name :encoding :utf-8)
                        :UTF8_STRING 8))

(defmethod port-set-mirror-name ((port clx-basic-port) mirror name)
  (%set-window-name mirror name)
  (%set-window-icon-name mirror name)
  (xlib:display-force-output (xlib:drawable-display mirror)))

(defmethod port-set-mirror-icon ((port clx-basic-port) mirror icon)
  ;; The format of the _NET_WM_ICON property is described in
  ;; "Application Window Properties" section of the "Extended Window
  ;; Manager Hints" specification:
  ;; https://specifications.freedesktop.org/wm-spec/1.5/ar01s05.html#idm45766085139216
  (let* ((width (pattern-width icon))
         (height (pattern-height icon))
         (pixel-count (* width height))
         (pixels (clime:pattern-array icon))
         (data (make-array (+ 2 pixel-count) :element-type '(unsigned-byte 32))))
    ;; The first two elements contain the width and the height
    ;; respectively. The remaining elements contain the icon pixels.
    (setf (aref data 0) width
          (aref data 1) height)
    (loop for i below pixel-count
          do (setf (aref data (+ i 2)) (row-major-aref pixels i)))
    (xlib:change-property mirror :_NET_WM_ICON data :cardinal 32)))

(defmethod port-set-mirror-region ((port clx-basic-port) mirror mirror-region)
  (with-bounding-rectangle* (x1 y1 x2 y2) mirror-region
    (declare (ignore x1 y1))
    (setf (xlib:drawable-width mirror) (round-coordinate x2)
          (xlib:drawable-height mirror) (round-coordinate y2))))

(defmethod port-set-mirror-transformation
    ((port clx-basic-port) mirror mirror-transformation)
  (multiple-value-bind (x y) (transform-position mirror-transformation 0 0)
    (setf (xlib:drawable-x mirror) (round-coordinate x)
          (xlib:drawable-y mirror) (round-coordinate y))))

(defmethod destroy-mirror ((port clx-basic-port) (sheet mirrored-sheet-mixin))
  (when (sheet-xmirror sheet)
    (xlib:destroy-window (sheet-xmirror sheet)))
  (when (port-lookup-mirror port sheet)
    (port-unregister-mirror port sheet (sheet-mirror sheet))))

(defmethod raise-mirror ((port clx-basic-port) (sheet basic-sheet))
  (let ((mirror (sheet-xmirror sheet)))
    (when (and mirror
               (typep mirror 'xlib:window))
      (xlib:circulate-window-up mirror))))

(defmethod bury-mirror ((port clx-basic-port) (sheet basic-sheet))
  (let ((mirror (sheet-xmirror sheet)))
    (when (and mirror
               (typep mirror 'xlib:window))
      (xlib:circulate-window-down mirror))))

(defmethod mirror-transformation ((port clx-basic-port) mirror)
  (make-translation-transformation (xlib:drawable-x mirror)
                                   (xlib:drawable-y mirror)))

(defmethod port-enable-sheet ((port clx-basic-port) (mirror mirrored-sheet-mixin))
  (xlib:map-window (sheet-direct-xmirror mirror)) )

(defmethod port-disable-sheet ((port clx-basic-port) (mirror mirrored-sheet-mixin))
  (xlib:unmap-window (sheet-direct-xmirror mirror)) )
