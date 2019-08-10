(cl:in-package #:clim-broadway)

(defclass surface-manager ()
  ((%surfaces :reader   surfaces
              :initform '())))
