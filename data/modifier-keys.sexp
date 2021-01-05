;; const name   value  key name  gesture constant
((+shift-key+   #x0100 "Shift"   :shift)
 (+control-key+ #x0200 "Control" :control)
 (+meta-key+    #x0400 "Meta"    :meta)
 (+super-key+   #x0800 "Super"   :super)
 (+hyper-key+   #x1000 "Hyper"   :hyper)
 (+alt-key+     #x2000 "Alt"     :unmapped)) ; `:unmapped' prevents `nil' match
