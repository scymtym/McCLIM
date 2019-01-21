;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Library General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Library General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Library General Public
;;;; License along with this library; if not, write to the
;;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;; Boston, MA  02111-1307  USA.

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
