(defsystem "clim-gadgets"
  :depends-on ("clim-basic" "clim-core")
  :serial t
  :components ((:file "protocol")
               (:file "theme")
               (:file "states")
               (:file "animation")
               (:file "gadgets")
               (:file "menu" :depends-on ("gadgets"))
               (:file "dialog" :depends-on ("gadgets"))
               (:file "dialog-views" :depends-on ("gadgets" "dialog"))))
