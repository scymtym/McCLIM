;;;; Copyright (C) 2018, 2019 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "new-inspector"
  :description "A graphical inspector for arbitrary Common Lisp objects."
  :license     "LGPL-2.1+"
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("mcclim")

  :components  ((:module     "base"
                 :pathname   "src"
                 :serial     t
                 :components ((:file       "package")

                              (:file       "formatting")

                              (:file       "protocol")

                              (:file       "place")
                              (:file       "place-formatting")

                              (:file       "presentations")
                              (:file       "commands")

                              (:file       "state")
                              (:file       "pane")))

                (:module     "navigation"
                 :pathname   "src/navigation"
                 :depends-on ("base")
                 :components ((:file       "navigation")))

                (:module     "objects"
                 :pathname   "src/objects"
                 :depends-on ("base")
                 :serial     t
                 :components ((:file       "generic")
                              (:file       "number")
                              (:file       "symbol")
                              (:file       "list")
                              (:file       "array")
                              (:file       "hash-table")
                              (:file       "instance")
                              (:file       "class")
                              (:file       "function")))

                (:module     "application"
                 :pathname   "src"
                 :depends-on ("base" "navigation")
                 :components ((:file       "application")))))
