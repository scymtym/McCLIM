(cl:in-package #:clim-clx)

;;; Double buffering sheet mixin

(defclass double-buffering-mixin (climi::double-buffering-mixin)
  ((%pixmap :accessor pixmap
            :initform nil)))

(defmethod port-set-mirror-region :after ((port clx-basic-port)
                                          (mirror t)
                                          mirror-region)
  (let ((sheet (port-lookup-sheet port mirror)))
    (when (typep sheet 'double-buffering-mixin)
      (let ((pixmap (pixmap sheet)))
        (flet ((allocate-new ()
                 (setf (pixmap sheet) (multiple-value-call #'allocate-pixmap
                                        sheet (bounding-rectangle-size mirror-region)))))
          (cond ((not pixmap)
                 (allocate-new))
                ((or (/= (pixmap-width pixmap)
                         (bounding-rectangle-width mirror-region))
                     (/= (pixmap-height pixmap)
                         (bounding-rectangle-height mirror-region)))
                 (deallocate-pixmap pixmap)
                 (allocate-new))))))))
