(in-package :clim-clx-fb)

(defclass framebuffer ()
  (;; Pixels and dirty region
   (%pixels       :accessor pixels       :initform nil)
   (%dirty-region :accessor dirty-regoin :initform nil)
   ;; X side
   (%x-pixels     :accessor x-pixels     :initform nil)
   (%x-image      :accessor x-image      :initform nil)))

(defun make-x-image (width height)
  (let* ((pixels (make-array (list height width)
                             :element-type '(unsigned-byte 32)
                             :initial-element #xffffffff))
         (image (xlib:create-image :bits-per-pixel 32
                                   :data pixels
                                   :depth 24
                                   :width width
                                   :height height
                                   :format :z-pixmap)))
    (values image pixels)))

(defclass clx-fb-mirror (image-mirror-mixin)
  ((width :initform 0)
   (height :initform 0)
   ;;
   (x-mirror :initform nil
             :initarg :x-mirror
             :reader x-mirror
             :reader sheet-direct-xmirror)
   (x-pixels :initform nil)
   (x-image :initform nil)
   (x-dirty-region :initform +nowhere+)
   (gcontext :initform nil)
   ;;
   (skip-count :initform 0)))

;;; for port
(defmethod mcclim-render-internals::%create-mirror-image :after ((sheet clx-fb-mirror) w h)
  (with-slots (mcclim-render-internals::dirty-region) sheet
    (setf mcclim-render-internals::dirty-region nil))
  ;;(let ((data (climi::pattern-array (image-mirror-image sheet))))
  (with-slots (width height x-pixels x-image) sheet
    (setf width w
          height h)
    (setf (values x-image x-pixels) (make-x-image width height))))

(defgeneric image-mirror-to-x (sheet))

(defmethod image-mirror-to-x ((sheet image-mirror-mixin))
  )

(defmethod image-mirror-to-x ((sheet xlib:window))
  )

(defun image-mirror-put (width height x-mirror gcontext x-image dirty-r)
  (declare (optimize speed))
  (let ((mirror-region (make-rectangle* 0 0 width height)))
    (if (and (typep dirty-r 'climi::standard-rectangle-set)
             (> (length (climi::standard-rectangle-set-bands dirty-r)) 4))
        (xlib::put-image x-mirror gcontext x-image ; TODO use the bounding-region
                         :src-x 0 :src-y 0 :x 0 :y 0 :width  width :height height)
        (map-over-region-set-regions
         (lambda (region)
           (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
               (region-intersection region mirror-region) ; TODO intersection not needed, can just clamp to [0,width], [0,height]
             (let ((width (round (- max-x min-x)))
                   (height (round (- max-y min-y)))
                   (min-x* (round min-x))
                   (min-y* (round min-y)))
               (when (and x-mirror x-image)
                 (xlib::put-image x-mirror
                                  gcontext
                                  x-image
                                  :src-x min-x* :src-y min-y*
                                  :x min-x* :y min-y*
                                  :width  (max 0 (- width (min 0 (- min-x))))
                                  :height (max 0 (- height (min 0 (- min-y)))))))))
         dirty-r))))

(defvar *screenshots* '())

(defun image-mirror-pre-put (width height x-mirror sheet x-image x-pixels dirty-r)
  (declare (type (simple-array (unsigned-byte 32) 2) x-pixels) ; TODO we have typedefs for this
           (optimize (speed 3) (debug 0) (safety 0)))
  ;; TODO can check whether dirty-r is completely within mirror-region
  (when (and x-mirror x-image)
    (let* ((pixels        (climi::pattern-array (image-mirror-image sheet)))
           (x-width       (array-dimension x-pixels 1))
           (mirror-region (make-rectangle* 0 0 width height))
           (fn (etypecase pixels
                 ((simple-array (unsigned-byte 32) 2)
                  (lambda (region)
                    (locally (declare (type (simple-array (unsigned-byte 32) 2) pixels))
                      (let ((pixels-width (array-dimension pixels 1)))
                        (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
                            (region-intersection region mirror-region)
                          (locally (declare (type (unsigned-byte 32) min-x min-y max-x max-y))
                            (mcclim-render-internals::do-region-pixels ((pixels-width si :x1 min-x :x2 max-x :y1 min-y :y2 max-y)
                                                                        (x-width      di :x1 min-x           :y1 min-y))
                              (setf (row-major-aref x-pixels di) (row-major-aref pixels si))))))))))))
      (map-over-region-set-regions fn dirty-r))))

(defmethod image-mirror-to-x ((sheet clx-fb-mirror))
  (declare (optimize speed))
  (with-slots (width height
               mcclim-render-internals::image-lock gcontext
               mcclim-render-internals::dirty-region
               mcclim-render-internals::finished-output
               x-mirror x-image x-dirty-region
               skip-count)
      sheet
    (when (not (region-equal x-dirty-region +nowhere+))
      (let ((reg))
        (clim-sys:with-lock-held (mcclim-render-internals::image-lock)
          (setf reg x-dirty-region)
          (setf x-dirty-region +nowhere+))
        (image-mirror-put width height x-mirror gcontext x-image reg)))))

(defmethod climb:port-set-mirror-name
    ((port clx-fb-port) (mirror clx-fb-mirror) name)
  (climb:port-set-mirror-name port (x-mirror mirror) name))

(defmethod climb:port-set-mirror-region ((port clx-fb-port) (mirror clx-fb-mirror) mirror-region)
  (climb:port-set-mirror-region port (x-mirror mirror) mirror-region))

(defmethod climb:port-set-mirror-transformation
    ((port clx-fb-port) (mirror clx-fb-mirror) mirror-transformation)
  (climb:port-set-mirror-transformation port (x-mirror mirror) mirror-transformation))

(defmethod mcclim-render-internals::%mirror-force-output ((mirror clx-fb-mirror))
  (with-slots (width height
               mcclim-render-internals::image-lock mcclim-render-internals::dirty-region
               x-mirror x-image x-pixels x-dirty-region)
      mirror
    (when mcclim-render-internals::dirty-region
      (clim-sys:with-lock-held (mcclim-render-internals::image-lock)
        (when mcclim-render-internals::dirty-region
          (setf x-dirty-region (region-union x-dirty-region mcclim-render-internals::dirty-region))
          (image-mirror-pre-put width height x-mirror mirror x-image x-pixels x-dirty-region)
          (setf mcclim-render-internals::dirty-region nil))))))
