;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2016 Alessandro Serra <gas2serra@gmail.com>
;;;  (c) copyright 2020 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; A specialized top-level pane for the raster image backend.

(in-package #:mcclim-raster-image)

(defclass raster-image-top-level-pane (;;sheet-mute-input-mixin
                                       sheet-mute-repainting-mixin
                                       image-sheet-mixin
                                       mirrored-sheet-mixin
                                       unmanaged-top-level-sheet-pane)
  ())
