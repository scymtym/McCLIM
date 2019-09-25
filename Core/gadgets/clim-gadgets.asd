(defsystem "clim-gadgets"
  :depends-on ("clim-basic" "clim-core")
  :components
  ((:file "gadgets")
   (:file "menu" :depends-on ("gadgets"))
   (:file "dialog" :depends-on ("gadgets"))
   (:file "dialog-views" :depends-on ("gadgets" "dialog"))))
