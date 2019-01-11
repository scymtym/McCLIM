(cl:defpackage #:new-inspector
  (:use
   #:clim-lisp
   #:alexandria
   #:clim)

  (:shadow
   #:inspect)

  (:shadowing-import-from #:alexandria
   #:simple-parse-error))
