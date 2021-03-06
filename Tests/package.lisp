(cl:defpackage #:clim-tests
  (:use
   #:clim-lisp
   #:clim
   #:clime
   #:fiveam)

  (:shadowing-import-from #:fiveam
   #:test)

  (:import-from #:climi
   #:coordinate=)

  (:import-from #:clim-test-util
   #:fails
   #:compilation-signals

   #:with-comparison-to-reference
   #:with-comparison-to-reference*)

  (:export #:run-tests))

(cl:in-package #:clim-tests)

(def-suite :mcclim)

(defun run-tests ()
  (run! :mcclim))
