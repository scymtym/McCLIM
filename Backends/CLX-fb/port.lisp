(in-package :clim-clx-fb)

(defclass clx-fb-port (render-port-mixin
		       clim-xcommon:keysym-port-mixin
		       clim-clx::clx-basic-port)
  ((%last-frame-time :accessor last-frame-time
                     :initform nil)))

(setf (get :clx-fb :port-type) 'clx-fb-port)
(setf (get :clx-fb :server-path-parser) 'clim-clx::parse-clx-server-path)

(defmethod initialize-instance :after ((port clx-fb-port) &rest args)
  (declare (ignore args))
  (push (make-instance 'clx-fb-frame-manager :port port)
	(slot-value port 'frame-managers))
  (setf (slot-value port 'pointer)
	(make-instance 'clim-clx::clx-basic-pointer :port port))
  (initialize-clx port)
  (clim-extensions:port-all-font-families port))

(defun swap-sheet-buffers (port)
  (handler-case
      (maphash
       (lambda (sheet mirror)
         (assert (eq mirror (sheet-mirror sheet))) ; TODO temp

         (when (typep sheet 'clx-fb-mirrored-sheet-mixin)
           (climi::invoke-with-suspended-sheet-event-processing
            (lambda ()
              (mcclim-render-internals::%mirror-force-output mirror)
              (image-mirror-to-x mirror))
            sheet)))
       (slot-value port 'climi::sheet->mirror))
    (condition (condition)
      (format *debug-io* "~A~%" condition)))
  (xlib:display-force-output (clx-port-display port))
  (setf (last-frame-time port) (get-internal-real-time)))

(defmethod process-next-event ((port clx-fb-port) &key wait-function (timeout nil))

  (let* ((frame-rate 30)
         (frame-period-time (/ frame-rate))
         (last-frame-time (last-frame-time port))
         (timeout (if timeout
                      (min frame-period-time timeout)
                      frame-period-time)))
    (call-next-method port :wait-function wait-function :timeout timeout)
    (when (or (null last-frame-time)
              (>= (- (get-internal-real-time) last-frame-time)
                  (* frame-period-time internal-time-units-per-second)))
      (swap-sheet-buffers port))))

(defparameter *event-mask* '(:exposure
			     :key-press :key-release
			     :button-press :button-release
			     :owner-grab-button
			     :enter-window :leave-window
			     :structure-notify
			     :pointer-motion :button-motion))

(defmethod clim-clx::realize-mirror ((port clx-fb-port) (sheet mirrored-sheet-mixin))
   (clim-clx::%realize-mirror port sheet)
   (port-register-mirror (port sheet) sheet (make-instance 'clx-fb-mirror :xmirror (port-lookup-mirror port sheet)))
   (port-lookup-mirror port sheet))

(defmethod clim-clx::realize-mirror ((port clx-fb-port) (pixmap pixmap))
  )

(defmethod clim-clx::%realize-mirror ((port clx-fb-port) (sheet basic-sheet))
  (clim-clx::realize-mirror-aux port sheet
		      :event-mask *event-mask*
                      :map (sheet-enabled-p sheet)))

(defmethod clim-clx::%realize-mirror ((port clx-fb-port) (sheet top-level-sheet-mixin))
  (let ((q (compose-space sheet)))
    (let ((frame (pane-frame sheet))
          (window (clim-clx::realize-mirror-aux port sheet
				      :event-mask *event-mask*
                                      :map nil
                                      :width (clim-clx::round-coordinate (space-requirement-width q))
                                      :height (clim-clx::round-coordinate (space-requirement-height q)))))
      (setf (xlib:wm-hints window) (xlib:make-wm-hints :input :on))
      (setf (xlib:wm-name window) (frame-pretty-name frame))
      (setf (xlib:wm-icon-name window) (frame-pretty-name frame))
      (xlib:set-wm-class
       window
       (string-downcase (frame-name frame))
       (string-capitalize (string-downcase (frame-name frame))))
      (setf (xlib:wm-protocols window) `(:wm_delete_window))
      (xlib:change-property window
                            :WM_CLIENT_LEADER (list (xlib:window-id window))
                            :WINDOW 32))))

(defmethod clim-clx::%realize-mirror ((port clx-fb-port) (sheet unmanaged-sheet-mixin))
  (clim-clx::realize-mirror-aux port sheet
		      :event-mask *event-mask*
		      :override-redirect :on
		      :map nil))



(defmethod make-medium ((port clx-fb-port) sheet)
  (make-instance 'clx-fb-medium
		 ;; :port port
		 ;; :graft (find-graft :port port)
		 :sheet sheet))


(defmethod make-graft ((port clx-fb-port) &key (orientation :default) (units :device))
  (let ((graft (make-instance 'clx-graft
		              :port port :mirror (clx-port-window port)
		              :orientation orientation :units units))
        (width (xlib:screen-width (clx-port-screen port)))
        (height (xlib:screen-height (clx-port-screen port))))
    (let ((region (make-bounding-rectangle 0 0 width height)))
      (climi::%%set-sheet-region region graft))
    graft))

(defmethod graft ((port clx-fb-port))
  (first (port-grafts port)))


(defmethod port-force-output ((port clx-fb-port))
  #+no (alexandria:maphash-keys
   (lambda (key)
     (when (typep key 'clx-fb-mirrored-sheet-mixin)
       (mcclim-render-internals::%mirror-force-output (sheet-mirror key))))
   (slot-value port 'climi::sheet->mirror))
  #+no (xlib:display-force-output (clx-port-display port)))

;;; Pixmap

(defmethod destroy-mirror ((port clx-fb-port) (pixmap image-pixmap-mixin))
  (call-next-method))

(defmethod realize-mirror ((port clx-fb-port) (pixmap image-pixmap-mixin))
  (setf (sheet-parent pixmap) (graft port))
  (let ((mirror (make-instance 'image-mirror-mixin)))
    (port-register-mirror port pixmap mirror)
    (mcclim-render-internals::%make-image mirror pixmap)))

(defmethod port-allocate-pixmap ((port clx-fb-port) sheet width height)
  (let ((pixmap (make-instance 'clx-fb-pixmap
			       :sheet sheet
			       :width width
			       :height height
			       :port port)))
    (when (sheet-grafted-p sheet)
      (realize-mirror port pixmap))
    pixmap))

(defmethod port-deallocate-pixmap ((port clx-fb-port) pixmap)
  (when (port-lookup-mirror port pixmap)
    (destroy-mirror port pixmap)))
