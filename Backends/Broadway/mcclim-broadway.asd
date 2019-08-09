(defsystem "mcclim-broadway"
  :depends-on ("nibbles"
               "utilities.binary-dump"
               "ironclad"
               "cl-base64"

               "usocket"

               "mcclim"
               "mcclim-render")

  :serial     t
  :components ((:file "package")

               (:module     "wire-protocol"
                :serial     t
                :components ((:file "meta-model")
                             (:file "definitions")
                             (:file "code-generation")
                             (:file "connection")))

               (:file "frame-manager")
               (:file "port")

               (:file "websocket") ; TODO make server stuff one module

               (:file "server")))
