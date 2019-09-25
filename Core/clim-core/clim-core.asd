(defsystem #:clim-core
  :depends-on (#:clim-basic #:clim-postscript #+sbcl (:require #:sb-introspect))
  :components
  ((:file "defresource")
   (:file "presentations")
   (:file "presentation-translators" :depends-on ("presentations"))
   (:file "bordered-output" :depends-on ("presentations"))
   (:file "table-formatting" :depends-on ("presentations"))
   (:file "input-editing" :depends-on ("presentations" "bordered-output" "table-formatting"))
   (:file "graph-formatting")
   (:file "frames" :depends-on ("commands" "presentations" "presentation-defs" "incremental-redisplay"))
   (:file "presentation-defs" :depends-on ("input-editing" "presentations"))
   (:file "describe" :depends-on ("presentations" "presentation-translators" "presentation-defs" "table-formatting"))
   (:file "commands" :depends-on ("input-editing" "presentations"
                                  "presentation-defs"))
   (:file "incremental-redisplay" :depends-on ("presentation-defs"))
   (:file "menu-choose" :depends-on ("commands" "table-formatting" "presentation-defs"
                                     "panes" "frames" "presentations"))
   (:file "panes" :depends-on ("incremental-redisplay" "presentations" "presentation-defs"
                               "input-editing" "frames"))
   (:file "builtin-commands" :depends-on ("table-formatting" "commands" "presentations"
                                          "presentation-defs" "input-editing"))))
