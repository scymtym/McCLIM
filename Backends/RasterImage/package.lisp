;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2016 Alessandro Serra <gas2serra@gmail.com>
;;;  (c) copyright 2018 Daniel Kochmanski <daniel@turtleware.eu>
;;;  (c) copyright 2019 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Package definition for the raster image backend.

(in-package #:common-lisp-user)

(defpackage #:mcclim-raster-image
  (:use #:clim #:clim-lisp #:clim-backend #:mcclim-render)
  (:import-from #:climi
   #:port-grafts
   #:updating-output-stream-mixin
   #:do-sequence
   #:with-transformed-position
   #:with-transformed-positions
   #:port-register-mirror
   #:port-unregister-mirror
   #:port-lookup-sheet
   #:destroy-mirror
   #:realize-mirror
   #:unmanaged-top-level-sheet-pane
   #:vbox-pane)
  (:import-from #:mcclim-render-internals
   #:render-medium-mixin
   #:render-port-mixin
   #:image-mirror-image
   #:image-sheet-mixin
   #:image-mirror-mixin
   #:image-pixmap-mixin
   #:%make-image
   #:image-mirror-image
   ;;#:save-image-to-file
   ;;#:save-image-to-stream
   )
  (:import-from #:mcclim-truetype
   #:glyph-pixarray
   #:ensure-gethash
   #:invoke-with-truetype-path-restart
   #:*truetype-font-path*
   #:*family-names*
   #:zpb-ttf-font-loader
   #:*zpb-font-lock*
   #:*fontconfig-faces*
   #:*families/faces*
   #:truetype-device-font-name
   #:fontconfig-font-name
   #:make-truetype-device-font-name
   #:make-fontconfig-font-name)
  (:export
   #:with-output-to-raster-image-stream
   #:with-output-to-raster-image-file
   #:with-output-to-rgba-pattern
   #:with-output-to-image-stream))
