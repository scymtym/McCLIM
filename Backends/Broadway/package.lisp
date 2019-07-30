(cl:defpackage #:clim-broadway
  (:use
   #:alexandria

   #:clim
   #:clim-lisp
   #:clim-backend)

  (:shadow
   #:type

   #:color)

  (:shadowing-import-from #:alexandria
   #:simple-parse-error))
