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

(cl:defpackage #:new-inspector
  (:use
   #:clim-lisp
   #:alexandria
   #:clim)

  (:shadow
   #:inspect)

  (:shadowing-import-from #:alexandria
   #:simple-parse-error)

  ;; Formatting Utilities
  (:export
   #:call-with-preserved-cursor-x      #:with-preserved-cursor-x
   #:call-with-preserved-cursor-y      #:with-preserved-cursor-y

   #:with-style
   #:call-with-section                 #:with-section
   #:call-with-placeholder-if-empty    #:with-placeholder-if-emtpy
   #:call-with-output-as-badge         #:with-output-as-badge
   #:badge

   #:call-with-safe-and-terse-printing #:with-safe-and-terse-printing
   #:call-with-error-handling          #:with-error-handling
                                       #:with-print-error-handling)

  ;; Place protocol and class
  (:export
   #:supportsp
   #:accepts-value-p
   #:valuep
   #:value                          ; also setf
   #:remove-value

   #:basic-place)

  ;; Object state protocol and class
  (:export
   #:place
   #:object
   #:state-applicable-p

   #:object-state-class
   #:make-object-state

   #:inspected-object)

  ;; Object inspection protocol
  (:export
   #:inspect-place
   #:inspect-object
   #:inspect-object-using-state)

  ;; Inspector state protocol
  (:export
   #:root-place                     ; also setf
   #:root-object                    ; also setf
   #:change-hook
   #:present-inspected-object-graph)

  ;; Inspector pane protocol
  (:export
   #:queue-redisplay)

  ;; User interface
  (:export
   #:inspect))
