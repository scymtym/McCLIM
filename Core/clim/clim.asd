(defsystem #:clim
  :depends-on (#:clim-core #:clim-gadgets #:drei-mcclim)
  :components ((:file "input-editing-drei")
               (:file "text-editor-gadget")))
