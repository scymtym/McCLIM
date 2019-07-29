(defsystem "clim-broadway"
  :depends-on ("nibbles"
               "utilities.binary-dump"
               "iconclad"
               "cl-base64"

               "usocket"

               "clim")

  :serial     t
  :components ((:file "package")
               (:file "server")))
