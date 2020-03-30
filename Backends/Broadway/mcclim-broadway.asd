;;;; (C) Copyright 2019, 2020 Jan Moringen
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

(defsystem "mcclim-broadway"
  :description "A backend for CLIM frames in a Webbrowser"
  :license     "LGPL-2.1+"
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :depends-on  ("nibbles"
                "utilities.binary-dump" ; TODO not anymore
                "ironclad"
                "cl-base64"             ; TODO not anymore

                "usocket"

                "mcclim"
                "mcclim-render")

  :serial      t
  :components  ((:file "package")

                (:module     "wire-protocol"
                 :serial     t
                 :components ((:file "meta-model")
                              (:file "definitions")

                              (:file "generate-lisp")
                              (:file "generate-javascript")

                              (:file "code-generation")

                              (:file "connection")
                              (:file "buffer")))

                (:module     "dom"
                 :serial     t
                 :components ((:file "node")
                              (:file "tile")
                              (:file "synchronization")
                              (:file "surface")
                              (:file "surface-manager")))

                (:file "medium")
                (:file "frame-manager")
                (:file "port")

                (:file "websocket") ; TODO make server stuff one module

                (:file "server")))
